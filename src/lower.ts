import {
  Ast,
  Expr,
  ExprBlock,
  FunctionDef,
  ImportDef,
  Item,
  LoopId,
  Resolution,
  Ty,
  TyFn,
  TyTuple,
  varUnreachable,
} from "./ast";
import { ComplexMap, encodeUtf8, unwrap } from "./utils";
import * as wasm from "./wasm/defs";

const USIZE: wasm.ValType = "i32";
// POINTERS ARE JUST INTEGERS
const POINTER: wasm.ValType = USIZE;

const STRING_TYPES: wasm.ValType[] = [POINTER, USIZE];
const STRING_ABI: ArgRetAbi = STRING_TYPES;

const WASM_PAGE = 65536;

type Relocation = {
  kind: "funccall";
  instr: wasm.Instr & { func: wasm.FuncIdx };
} & { res: Resolution };

type FuncOrImport =
  | { kind: "func"; idx: wasm.FuncIdx }
  | { kind: "import"; idx: number };

export type Context = {
  mod: wasm.Module;
  funcTypes: ComplexMap<wasm.FuncType, wasm.TypeIdx>;
  reservedHeapMemoryStart: number;
  funcIndices: ComplexMap<Resolution, FuncOrImport>;
  ast: Ast;
  relocations: Relocation[];
};

function mangleDefPath(defPath: string[]): string {
  return `nil__${defPath.map(escapeIdentName).join("__")}`;
}

function escapeIdentName(name: string): string {
  // This allows the implementation to use 2 leading underscores
  // to separate ident parts of in a loading position and avoid conflicts.
  return name.replace(/__/g, "___");
}

function internFuncType(cx: Context, type: wasm.FuncType): wasm.TypeIdx {
  const existing = cx.funcTypes.get(type);
  if (existing !== undefined) {
    return existing;
  }
  const idx = cx.mod.types.length;
  cx.mod.types.push(type);
  cx.funcTypes.set(type, idx);
  return idx;
}

function appendData(cx: Context, newData: Uint8Array): number {
  const datas = cx.mod.datas;

  if (datas.length === 0) {
    datas.push({
      init: newData,
      mode: {
        kind: "active",
        memory: 0,
        offset: [{ kind: "i32.const", imm: 0 }],
      },
      _name: "staticdata",
    });
    return 0;
  } else {
    const data = datas[0];
    const idx = data.init.length;
    const init = new Uint8Array(data.init.length + newData.length);
    init.set(data.init, 0);
    init.set(newData, data.init.length);
    data.init = init;
    return idx;
  }
}

export function lower(ast: Ast): wasm.Module {
  const mod: wasm.Module = {
    types: [],
    funcs: [],
    tables: [],
    mems: [],
    globals: [],
    elems: [],
    datas: [],
    imports: [],
    exports: [],
  };

  mod.mems.push({ _name: "memory", type: { min: WASM_PAGE, max: WASM_PAGE } });
  mod.exports.push({ name: "memory", desc: { kind: "memory", idx: 0 } });

  mod.tables.push({
    _name: "__indirect_function_table",
    type: { limits: { min: 0, max: 0 }, reftype: "funcref" },
  });
  mod.exports.push({
    name: "__indirect_function_table",
    desc: { kind: "table", idx: 0 },
  });

  const cx: Context = {
    mod,
    funcTypes: new ComplexMap(),
    funcIndices: new ComplexMap(),
    reservedHeapMemoryStart: 0,
    ast,
    relocations: [],
  };

  function lowerMod(items: Item[]) {
    items.forEach((item) => {
      switch (item.kind) {
        case "function": {
          lowerFunc(cx, item, item.node);
          break;
        }
        case "import": {
          lowerImport(cx, item, item.node);
          break;
        }
        case "mod": {
          if (item.node.modKind.kind === "inline") {
            lowerMod(item.node.modKind.contents);
          }
        }
      }
    });
  }

  lowerMod(ast.rootItems);

  const HEAP_ALIGN = 0x08;
  cx.reservedHeapMemoryStart =
    mod.datas.length > 0
      ? (mod.datas[0].init.length + (HEAP_ALIGN - 1)) & ~(HEAP_ALIGN - 1)
      : 0;

  addRt(cx, ast);

  // THE LINKER
  const offset = cx.mod.imports.length;
  cx.relocations.forEach((rel) => {
    switch (rel.kind) {
      case "funccall": {
        const idx = cx.funcIndices.get(rel.res);
        if (idx === undefined) {
          throw new Error(
            `no function found for relocation '${JSON.stringify(rel.res)}'`
          );
        }
        rel.instr.func = idx.kind === "func" ? offset + idx.idx : idx.idx;
      }
    }
  });
  // END OF THE LINKER

  return mod;
}

function lowerImport(cx: Context, item: Item, def: ImportDef) {
  const existing = cx.mod.imports.findIndex(
    (imp) => imp.module === def.module.value && imp.name === def.func.value
  );

  let idx;
  if (existing !== -1) {
    idx = existing;
  } else {
    const abi = computeAbi(def.ty!);
    const { type: wasmType } = wasmTypeForAbi(abi);
    const type = internFuncType(cx, wasmType);

    idx = cx.mod.imports.length;
    cx.mod.imports.push({
      module: def.module.value,
      name: def.func.value,
      desc: {
        kind: "func",
        type,
      },
    });
  }

  cx.funcIndices.set({ kind: "item", id: item.id }, { kind: "import", idx });
}

type FuncContext = {
  cx: Context;
  item: Item;
  func: FunctionDef;
  wasmType: wasm.FuncType;
  wasm: wasm.Func;
  varLocations: VarLocation[];
  loopDepths: Map<LoopId, number>;
  currentBlockDepth: number;
};

type FnAbi = { params: ArgRetAbi[]; ret: ArgRetAbi };

type ArgRetAbi = wasm.ValType[];

type VarLocation = { localIdx: number; types: wasm.ValType[] };

function lowerFunc(cx: Context, item: Item, func: FunctionDef) {
  const abi = computeAbi(func.ty!);
  const { type: wasmType, paramLocations } = wasmTypeForAbi(abi);
  const type = internFuncType(cx, wasmType);

  const wasmFunc: wasm.Func = {
    _name: mangleDefPath(item.defPath!),
    type,
    locals: [],
    body: [],
  };

  const fcx: FuncContext = {
    cx,
    item,
    func,
    wasmType,
    wasm: wasmFunc,
    varLocations: paramLocations,
    loopDepths: new Map(),
    currentBlockDepth: 0,
  };

  lowerExpr(fcx, wasmFunc.body, fcx.func.body);

  const idx = fcx.cx.mod.funcs.length;
  fcx.cx.mod.funcs.push(wasmFunc);

  fcx.cx.funcIndices.set(
    { kind: "item", id: fcx.item.id },
    { kind: "func", idx }
  );
}

/*
Expression lowering.
- the result of an expression evaluation is stored on the top of the stack
*/

function lowerExpr(fcx: FuncContext, instrs: wasm.Instr[], expr: Expr) {
  const ty = expr.ty!;

  exprKind: switch (expr.kind) {
    case "empty": {
      // A ZST, do nothing.
      return;
    }
    case "let": {
      lowerExpr(fcx, instrs, expr.rhs);
      const types = wasmTypeForBody(expr.rhs.ty!);

      const local = fcx.wasm.locals.length + fcx.wasmType.params.length;

      fcx.wasm.locals.push(...types);

      types.forEach((_, i) => {
        instrs.push({ kind: "local.set", imm: local + i });
      });

      fcx.varLocations.push({ localIdx: local, types });

      break;
    }
    case "assign": {
      lowerExpr(fcx, instrs, expr.rhs);
      const { lhs } = expr;
      switch (lhs.kind) {
        case "ident": {
          const res = lhs.value.res!;

          switch (res.kind) {
            case "local": {
              const location =
                fcx.varLocations[fcx.varLocations.length - 1 - res.index];
              storeVariable(instrs, location);
              break;
            }
            case "item": {
              throw new Error("cannot store to item");
            }
            case "builtin": {
              throw new Error("cannot store to builtin");
            }
          }
          break;
        }
        default: {
          throw new Error("invalid lhs side of assignment");
        }
      }

      break;
    }
    case "block": {
      const prevVarLocationLengths = fcx.varLocations.length;

      if (expr.exprs.length === 0) {
        // do nothing
      } else if (expr.exprs.length === 1) {
        lowerExpr(fcx, instrs, expr.exprs[0]);
      } else {
        const instr: wasm.Instr = {
          kind: "block",
          instrs: lowerExprBlockBody(fcx, expr),
          type: blockTypeForBody(fcx.cx, expr.ty!),
        };

        instrs.push(instr);
      }

      fcx.varLocations.length = prevVarLocationLengths;
      break;
    }
    case "literal": {
      switch (expr.value.kind) {
        case "str":
          const utf8 = encodeUtf8(expr.value.value);
          const idx = appendData(fcx.cx, utf8);

          instrs.push({ kind: "i32.const", imm: idx });
          instrs.push({ kind: "i32.const", imm: utf8.length });

          break;
        case "int":
          switch (expr.value.type) {
            case "Int":
              instrs.push({ kind: "i64.const", imm: expr.value.value });
              break;
            case "I32":
              instrs.push({ kind: "i32.const", imm: expr.value.value });
              break;
          }
          break;
      }
      break;
    }
    case "path":
    case "ident": {
      const res = expr.kind === "ident" ? expr.value.res! : expr.res;

      switch (res.kind) {
        case "local": {
          const location =
            fcx.varLocations[fcx.varLocations.length - 1 - res.index];
          loadVariable(instrs, location);
          break;
        }
        case "item":
          todo("item ident res");
        case "builtin":
          switch (res.name) {
            case "false":
              instrs.push({ kind: "i32.const", imm: 0 });
              break;
            case "true":
              instrs.push({ kind: "i32.const", imm: 1 });
              break;
            case "print":
              todo("print function");
            default: {
              throw new Error(`${res.name}#B is not a value`);
            }
          }
      }

      break;
    }
    case "binary": {
      // By evaluating the LHS first, the RHS is on top, which
      // is correct as it's popped first. Evaluating the LHS first
      // is correct for the source language too so great, no swapping.
      lowerExpr(fcx, instrs, expr.lhs);
      lowerExpr(fcx, instrs, expr.rhs);

      const lhsTy = expr.lhs.ty!;
      const rhsTy = expr.rhs.ty!;
      if (
        (lhsTy.kind === "int" && rhsTy.kind === "int") ||
        (lhsTy.kind === "i32" && rhsTy.kind === "i32")
      ) {
        let kind: wasm.Instr["kind"];
        const valty = lhsTy.kind === "int" ? "i64" : "i32";
        switch (expr.binaryKind) {
          case "+":
            kind = `${valty}.add`;
            break;
          case "-":
            kind = `${valty}.sub`;
            break;
          case "*":
            kind = `${valty}.mul`;
            break;
          case "/":
            kind = `${valty}.div_u`;
            break;
          case "%":
            kind = `${valty}.rem_u`;
            break;
          case "&":
            kind = `${valty}.and`;
            break;
          case "|":
            kind = `${valty}.or`;
            break;
          case "<":
            kind = `${valty}.lt_u`;
            break;
          case ">":
            kind = `${valty}.gt_u`;
            // errs
            break;
          case "==":
            kind = `${valty}.eq`;
            break;
          case "<=":
            kind = `${valty}.le_u`;
            break;
          case ">=":
            kind = `${valty}.ge_u`;
            break;
          case "!=":
            kind = `${valty}.ne`;
            break;
        }
        const instr: wasm.NumericInstr = { kind } as any; // Typescript is buggy.
        instrs.push(instr);
      } else if (lhsTy.kind === "bool" && rhsTy.kind === "bool") {
        let kind: wasm.Instr["kind"];

        switch (expr.binaryKind) {
          case "&":
            kind = "i32.and";
            break;
          case "|":
            kind = "i32.or";
            break;
          case "==":
            kind = "i32.eq";
            break;
          case "!=":
            kind = "i32.ne";
            break;
          case "<":
          case ">":
          case "<=":
          case ">=":
          case "+":
          case "-":
          case "*":
          case "/":
          case "%":
            throw new Error(`Invalid bool binary expr: ${expr.binaryKind}`);
        }

        instrs.push({ kind });
      } else {
        todo("non int/bool binary expr");
      }

      break;
    }
    case "unary": {
      lowerExpr(fcx, instrs, expr.rhs);
      switch (expr.unaryKind) {
        case "!":
          if (ty.kind === "bool") {
            // `xor RHS, 1` flips the lowermost bit.
            instrs.push({ kind: "i64.const", imm: 1 });
            instrs.push({ kind: "i64.xor" });
          } else if (ty.kind === "int") {
            // `xor RHS, -1` flips all bits.
            todo("Thanks to JS, we cannot represent -1 i64 yet");
          }
          break;
        case "-":
          todo("negation");
      }
      break;
    }
    case "call": {
      if (expr.lhs.kind !== "ident" && expr.lhs.kind !== "path") {
        todo("non constant calls");
      }

      const res =
        expr.lhs.kind === "ident" ? expr.lhs.value.res! : expr.lhs.res;

      if (res.kind === "builtin") {
        switch (res.name) {
          case "trap": {
            instrs.push({ kind: "unreachable" });
            break exprKind;
          }
          case "__i32_load": {
            lowerExpr(fcx, instrs, expr.args[0]);
            instrs.push({ kind: "i64.load", imm: {} });
            break exprKind;
          }
          case "__i64_load": {
            lowerExpr(fcx, instrs, expr.args[0]);
            instrs.push({ kind: "i64.load", imm: {} });
            break exprKind;
          }
          case "__i32_store": {
            lowerExpr(fcx, instrs, expr.args[0]);
            lowerExpr(fcx, instrs, expr.args[1]);
            instrs.push({ kind: "i32.store", imm: {} });
            break exprKind;
          }
          case "__i64_store": {
            lowerExpr(fcx, instrs, expr.args[0]);
            lowerExpr(fcx, instrs, expr.args[1]);
            instrs.push({ kind: "i64.store", imm: {} });
            break exprKind;
          }
          case "__string_ptr": {
            lowerExpr(fcx, instrs, expr.args[0]);
            // ptr, len
            instrs.push({ kind: "drop" });
            // ptr
            break exprKind;
          }
          case "__string_len": {
            lowerExpr(fcx, instrs, expr.args[0]);
            // ptr, len
            instrs.push({ kind: "i32.const", imm: 0 });
            // ptr, len, 0
            instrs.push({ kind: "select" });
            // len
            break exprKind;
          }
        }
      }

      const callInstr: wasm.Instr = { kind: "call", func: 9999999999 };
      fcx.cx.relocations.push({
        kind: "funccall",
        instr: callInstr,
        res,
      });

      expr.args.forEach((arg) => {
        lowerExpr(fcx, instrs, arg);
      });
      instrs.push(callInstr);
      break;
    }
    case "fieldAccess": {
      // We could just naively always evaluate the LHS normally, but that's kinda
      // stupid as it would cause way too much code for `let a = (0, 0, 0); a.0`
      // as that operation would first load the entire tuple onto the stack!
      // Therefore, we are a little clever be peeking into the LHS and doing
      // something smarter if it's another field access or ident (in the future,
      // we should be able to generalize this to all "places"/"lvalues").

      // TODO: Actually do this instead of being naive.

      const isPlace = (expr: Expr) =>
        expr.kind === "ident" || expr.kind === "fieldAccess";

      function project() {}

      lowerExpr(fcx, instrs, expr.lhs);

      switch (expr.lhs.ty!.kind) {
        case "tuple": {
          // Tuples have a by-value ABI, so we can simply index.
          const lhsSize = argRetAbi(expr.lhs.ty!).length;
          const resultAbi = argRetAbi(expr.ty!);
          const resultSize = resultAbi.length;
          const wasmIdx = wasmTypeIdxForTupleField(
            expr.lhs.ty!,
            expr.field.fieldIdx!
          );

          // lhsSize=5, resultSize=2, wasmIdx=2
          // I I Y Y I
          // drop, 2xlocal.set, drop, drop, 2xlocal.get

          // TODO: Establish some way of having reusable "scratch locals".
          const localIdx = fcx.wasm.locals.length + fcx.wasmType.params.length;
          fcx.wasm.locals.push(...resultAbi);

          Array(lhsSize - wasmIdx - resultSize)
            .fill(0)
            .forEach(() => instrs.push({ kind: "drop" }));

          if (expr.field.fieldIdx! > 0) {
            // Keep the result in scratch space.
            storeVariable(instrs, { localIdx, types: resultAbi });

            Array(wasmIdx)
              .fill(0)
              .forEach(() => instrs.push({ kind: "drop" }));

            loadVariable(instrs, { localIdx, types: resultAbi });
          }

          break;
        }
        case "struct": {
          todo("struct field accesses");
        }
        default:
          throw new Error("invalid field access lhs");
      }

      break;
    }
    case "if": {
      lowerExpr(fcx, instrs, expr.cond!);

      fcx.currentBlockDepth++;
      const thenInstrs: wasm.Instr[] = [];
      lowerExpr(fcx, thenInstrs, expr.then);

      const elseInstrs: wasm.Instr[] = [];
      // If there is no else, the type is (), so an empty instr array is correct.
      if (expr.else) {
        lowerExpr(fcx, elseInstrs, expr.else);
      }
      fcx.currentBlockDepth--;

      instrs.push({
        kind: "if",
        then: thenInstrs,
        else: elseInstrs,
        type: blockTypeForBody(fcx.cx, expr.ty!),
      });

      break;
    }
    case "loop": {
      const outerBlockInstrs: wasm.Instr[] = [];

      fcx.loopDepths.set(expr.loopId, fcx.currentBlockDepth);
      fcx.currentBlockDepth += 2;
      const bodyInstrs: wasm.Instr[] = [];
      lowerExpr(fcx, bodyInstrs, expr.body);

      // For diverging bodies, we don't need to bother creating the back edge.
      if (expr.body.ty!.kind !== "never") {
        bodyInstrs.push({
          kind: "br",
          label: /*innermost control structure, the loop*/ 0,
        });
      }
      fcx.currentBlockDepth -= 2;

      outerBlockInstrs.push({
        kind: "loop",
        instrs: bodyInstrs,
        type: blockTypeForBody(fcx.cx, expr.ty!),
      });

      instrs.push({
        kind: "block",
        instrs: outerBlockInstrs,
        type: blockTypeForBody(fcx.cx, expr.ty!),
      });
      fcx.loopDepths.delete(expr.loopId);

      break;
    }
    case "break": {
      const loopDepth = unwrap(fcx.loopDepths.get(expr.target!));

      console.log(loopDepth, fcx.currentBlockDepth);

      instrs.push({
        kind: "br",
        label: fcx.currentBlockDepth - loopDepth - 1,
      });
      break;
    }
    case "structLiteral": {
      todo("struct literal");
    }
    case "tupleLiteral": {
      expr.fields.forEach((field) => lowerExpr(fcx, instrs, field));
      break;
    }
    default: {
      const _: never = expr;
    }
  }

  if (ty.kind === "never") {
    instrs.push({ kind: "unreachable" });
    return;
  }
}

function lowerExprBlockBody(
  fcx: FuncContext,
  expr: ExprBlock & Expr
): wasm.Instr[] {
  fcx.currentBlockDepth++;
  const innerInstrs: wasm.Instr[] = [];

  const headExprs = expr.exprs.slice(0, -1);
  const tailExpr = expr.exprs[expr.exprs.length - 1];

  for (const inner of headExprs) {
    lowerExpr(fcx, innerInstrs, inner);
    if (inner.ty!.kind === "never") {
      // The rest of the block is unreachable, so we don't bother codegening it.
      break;
    }
    const types = wasmTypeForBody(inner.ty!);
    types.forEach(() => innerInstrs.push({ kind: "drop" }));
  }

  lowerExpr(fcx, innerInstrs, tailExpr);

  fcx.currentBlockDepth--;

  return innerInstrs;
}

function loadVariable(instrs: wasm.Instr[], loc: VarLocation) {
  // If the type is a ZST we'll have no types and do the following:
  // Load the ZST:
  // ...
  // 🪄 poof, the ZST is on the stack now.
  // --
  // Otherwise, load each part.
  loc.types.forEach((_, i) => {
    instrs.push({ kind: "local.get", imm: loc.localIdx + i });
  });
}

function storeVariable(instrs: wasm.Instr[], loc: VarLocation) {
  // Stores are just like loads, just the other way around.
  const types = loc.types.map((_, i) => i);
  types.reverse();
  types.forEach((i) => {
    instrs.push({ kind: "local.set", imm: loc.localIdx + i });
  });
}

function argRetAbi(param: Ty): ArgRetAbi {
  switch (param.kind) {
    case "string":
      return STRING_ABI;
    case "fn":
      todo("fn abi");
    case "int":
      return ["i64"];
    case "i32":
      return ["i32"];
    case "bool":
      return ["i32"];
    case "list":
      todo("list abi");
    case "tuple":
      return param.elems.flatMap(argRetAbi);
    case "struct":
      todo("struct ABI");
    case "never":
      return [];
    case "var":
      varUnreachable();
  }
}

function computeAbi(ty: TyFn): FnAbi {
  const params = ty.params.map(argRetAbi);
  const ret = argRetAbi(ty.returnTy);

  return { params, ret };
}

function wasmTypeForAbi(abi: FnAbi): {
  type: wasm.FuncType;
  paramLocations: VarLocation[];
} {
  const params: wasm.ValType[] = [];
  const paramLocations: VarLocation[] = [];

  abi.params.forEach((arg) => {
    paramLocations.push({
      localIdx: params.length,
      types: arg,
    });
    params.push(...arg);
  });

  return { type: { params, returns: abi.ret }, paramLocations };
}

function wasmTypeForBody(ty: Ty): wasm.ValType[] {
  switch (ty.kind) {
    case "string":
      return STRING_TYPES;
    case "int":
      return ["i64"];
    case "i32":
      return ["i32"];
    case "bool":
      return ["i32"];
    case "list":
      todo("list types");
    case "tuple":
      return ty.elems.flatMap(wasmTypeForBody);
    case "fn":
      todo("fn types");
    case "struct":
      todo("struct types");
    case "never":
      return [];
    case "var":
      varUnreachable();
  }
}

function blockTypeForBody(cx: Context, ty: Ty): wasm.Blocktype {
  const typeIdx = internFuncType(cx, {
    params: [],
    returns: wasmTypeForBody(ty),
  });
  return { kind: "typeidx", idx: typeIdx };
}

function wasmTypeIdxForTupleField(ty: TyTuple, idx: number): number {
  // Tuples are all flattened by value, so we just count the values in
  // the flattened representation.
  const layout = ty.elems.map(argRetAbi);
  const head = layout.slice(0, idx);
  return head.reduce((a, b) => a + b.length, 0);
}

function todo(msg: string): never {
  throw new Error(`TODO: ${msg}`);
}

// Make the program runnable using wasi-preview-1
function addRt(cx: Context, ast: Ast) {
  const { mod } = cx;

  const mainCall: wasm.Instr = { kind: "call", func: 9999999 };
  cx.relocations.push({
    kind: "funccall",
    instr: mainCall,
    res: ast.typeckResults!.main,
  });

  const start: wasm.Func = {
    _name: "_start",
    type: internFuncType(cx, { params: [], returns: [] }),
    locals: [],
    body: [mainCall],
  };

  const startIdx = mod.funcs.length;
  mod.funcs.push(start);

  const reserveMemory = (amount: number) => {
    const start = cx.reservedHeapMemoryStart;
    cx.reservedHeapMemoryStart += amount;
    return start;
  };

  const fd_write_type = internFuncType(cx, {
    params: ["i32", "i32", "i32", "i32"],
    returns: ["i32"],
  });

  cx.mod.imports.push({
    module: "wasi_snapshot_preview1",
    name: "fd_write",
    desc: { kind: "func", type: fd_write_type },
  });

  const printReturnValue = reserveMemory(4);
  const iovecArray = reserveMemory(8);

  const print: wasm.Func = {
    _name: "nil__print",
    locals: [],
    type: internFuncType(cx, { params: [POINTER, USIZE], returns: [] }),
    body: [
      // get the pointer and store it in the iovec
      { kind: "i32.const", imm: iovecArray },
      { kind: "local.get", imm: 0 },
      { kind: "i32.store", imm: { offset: 0, align: 4 } },
      // get the length and store it in the iovec
      { kind: "i32.const", imm: iovecArray + 4 },
      { kind: "local.get", imm: 1 },
      { kind: "i32.store", imm: { offset: 0, align: 4 } },
      // now call stuff
      { kind: "i32.const", imm: /*stdout*/ 1 },
      { kind: "i32.const", imm: iovecArray },
      { kind: "i32.const", imm: /*iovec len*/ 1 },
      { kind: "i32.const", imm: /*out ptr*/ printReturnValue },
      { kind: "call", func: 0 },
      { kind: "drop" },
    ],
  };
  const printIdx = cx.mod.funcs.length;
  cx.mod.funcs.push(print);

  cx.funcIndices.set(
    { kind: "builtin", name: "print" },
    { kind: "func", idx: printIdx }
  );

  mod.exports.push({
    name: "_start",
    desc: { kind: "func", idx: startIdx + mod.imports.length },
  });
}
