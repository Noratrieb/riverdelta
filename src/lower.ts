import {
  Ast,
  Expr,
  ExprBlock,
  FunctionDef,
  Item,
  Resolution,
  Ty,
  TyFn,
  varUnreachable,
} from "./ast";
import { encodeUtf8 } from "./utils";
import * as wasm from "./wasm/defs";

type StringifiedForMap<T> = string;

const USIZE: wasm.ValType = "i32";
// POINTERS ARE JUST INTEGERS
const POINTER: wasm.ValType = USIZE;

const STRING_TYPES: wasm.ValType[] = [POINTER, USIZE];
const STRING_ABI: ArgRetAbi = {
  kind: "aggregate",
  types: STRING_TYPES,
};

const WASM_PAGE = 65536;

type Relocation = {
  kind: "funccall";
  instr: wasm.Instr & { func: wasm.FuncIdx };
} & { res: Resolution };

function setMap<K, V>(map: Map<StringifiedForMap<K>, V>, key: K, value: V) {
  map.set(JSON.stringify(key), value);
}

function getMap<K, V>(
  map: Map<StringifiedForMap<K>, V>,
  key: K
): V | undefined {
  return map.get(JSON.stringify(key));
}

export type Context = {
  mod: wasm.Module;
  funcTypes: Map<StringifiedForMap<wasm.FuncType>, wasm.TypeIdx>;
  reservedHeapMemoryStart: number;
  funcIndices: Map<StringifiedForMap<Resolution>, wasm.FuncIdx>;
  ast: Ast;
  relocations: Relocation[];
};

function escapeIdentName(name: string): string {
  // this allows the implementation to use an odd number of leading
  // underscores
  return name.replace(/_/g, "__");
}

function internFuncType(cx: Context, type: wasm.FuncType): wasm.TypeIdx {
  const existing = getMap(cx.funcTypes, type);
  if (existing !== undefined) {
    return existing;
  }
  const idx = cx.mod.types.length;
  cx.mod.types.push(type);
  setMap(cx.funcTypes, type, idx);
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
    funcTypes: new Map(),
    funcIndices: new Map(),
    reservedHeapMemoryStart: 0,
    ast,
    relocations: [],
  };

  ast.items.forEach((item) => {
    switch (item.kind) {
      case "function": {
        lowerFunc(cx, item, item.node);
      }
    }
  });

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
        const idx = getMap<Resolution, number>(cx.funcIndices, rel.res);
        if (idx === undefined) {
          throw new Error(
            `no function found for relocation '${JSON.stringify(rel.res)}'`
          );
        }
        rel.instr.func = offset + idx;
      }
    }
  });
  // END OF THE LINKER

  return mod;
}

type FuncContext = {
  cx: Context;
  item: Item;
  func: FunctionDef;
  wasm: wasm.Func;
  varLocations: VarLocation[];
};

type FnAbi = { params: ArgRetAbi[]; ret: ArgRetAbi };

type ArgRetAbi =
  | { kind: "scalar"; type: wasm.ValType }
  | { kind: "zst" }
  | { kind: "aggregate"; types: wasm.ValType[] };

type VarLocation = { kind: "local"; idx: number } | { kind: "zst" };

function lowerFunc(cx: Context, item: Item, func: FunctionDef) {
  const abi = computeAbi(func.ty!);
  const { type: wasmType, paramLocations } = wasmTypeForAbi(abi);
  const type = internFuncType(cx, wasmType);

  const wasmFunc: wasm.Func = {
    _name: escapeIdentName(func.name),
    type,
    locals: [],
    body: [],
  };

  const fcx: FuncContext = {
    cx,
    item,
    func,
    wasm: wasmFunc,
    varLocations: paramLocations,
  };

  lowerExpr(fcx, wasmFunc.body, fcx.func.body);

  const idx = fcx.cx.mod.funcs.length;
  fcx.cx.mod.funcs.push(wasmFunc);
  setMap<Resolution, number>(
    fcx.cx.funcIndices,
    { kind: "item", index: fcx.item.id },
    idx
  );
}

/*
Expression lowering.
- the result of an expression evaluation is stored on the top of the stack
*/

function lowerExpr(fcx: FuncContext, instrs: wasm.Instr[], expr: Expr) {
  const ty = expr.ty!;

  switch (expr.kind) {
    case "empty": {
      // A ZST, do nothing.
      return;
    }
    case "let": {
      lowerExpr(fcx, instrs, expr.rhs);
      const type = wasmTypeForBody(expr.rhs.ty!);
      if (type.length === 0) {
        fcx.varLocations.push({ kind: "zst" });
      } else if (type.length === 1) {
        const local = fcx.wasm.locals.length;
        fcx.wasm.locals.push(type[0]);

        instrs.push({ kind: "local.set", imm: local });

        fcx.varLocations.push({ kind: "local", idx: local });
      } else {
        todo("complex locals");
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
          instrs.push({ kind: "i64.const", imm: expr.value.value });
          break;
      }
      break;
    }
    case "ident": {
      const res = expr.value.res!;
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

      if (expr.lhs.ty!.kind === "int" && expr.rhs.ty!.kind === "int") {
        let kind: wasm.Instr["kind"];
        switch (expr.binaryKind) {
          case "+":
            kind = "i64.add";
            break;
          case "-":
            kind = "i64.sub";
            break;
          case "*":
            kind = "i64.mul";
            break;
          case "/":
            kind = "i64.div_u";
            break;
          case "&":
            kind = "i64.and";
            break;
          case "|":
            kind = "i64.or";
            break;
          case "<":
            kind = "i64.lt_u";
            break;
          case ">":
            kind = "i64.gt_u";
            // errs
            break;
          case "==":
            kind = "i64.eq";
            break;
          case "<=":
            kind = "i64.le_u";
            break;
          case ">=":
            kind = "i64.ge_u";
            break;
          case "!=":
            kind = "i64.ne";
            break;
        }
        instrs.push({ kind });
      } else if (expr.lhs.ty!.kind === "bool" && expr.rhs.ty!.kind === "bool") {
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
      if (expr.lhs.kind !== "ident") {
        todo("non constant calls");
      }
      const callInstr: wasm.Instr = { kind: "call", func: 9999999999 };
      fcx.cx.relocations.push({
        kind: "funccall",
        instr: callInstr,
        res: expr.lhs.value.res!,
      });

      expr.args.forEach((arg) => {
        lowerExpr(fcx, instrs, arg);
      });
      instrs.push(callInstr);
      break;
    }
    case "if": {
      lowerExpr(fcx, instrs, expr.cond!);

      const thenInstrs: wasm.Instr[] = [];
      lowerExpr(fcx, thenInstrs, expr.then);

      const elseInstrs: wasm.Instr[] = [];
      // If there is no else, the type is (), so an empty instr array is correct.
      if (expr.else) {
        lowerExpr(fcx, elseInstrs, expr.else);
      }

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

      const bodyInstrs: wasm.Instr[] = [];
      lowerExpr(fcx, bodyInstrs, expr.body);
      bodyInstrs.push({
        kind: "br",
        label: /*innermost control structure, the loop*/ 0,
      });

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

      break;
    }
    case "break": {
      instrs.push({
        kind: "br",
        label: expr.target! + /* the block outside the loop */ 1,
      });
      break;
    }
    case "structLiteral": {
      todo("struct literal");
    }
    default: {
      const _: never = expr;
    }
  }
}

function lowerExprBlockBody(
  fcx: FuncContext,
  expr: ExprBlock & Expr
): wasm.Instr[] {
  const innerInstrs: wasm.Instr[] = [];

  const headExprs = expr.exprs.slice(0, -1);
  const tailExpr = expr.exprs[expr.exprs.length - 1];

  headExprs.forEach((inner) => {
    lowerExpr(fcx, innerInstrs, inner);
    const types = wasmTypeForBody(inner.ty!);
    types.forEach(() => innerInstrs.push({ kind: "drop" }));
  });

  lowerExpr(fcx, innerInstrs, tailExpr);

  return innerInstrs;
}

function loadVariable(instrs: wasm.Instr[], loc: VarLocation) {
  switch (loc.kind) {
    case "local": {
      instrs.push({ kind: "local.get", imm: loc.idx });
      break;
    }
    case "zst":
      // Load the ZST:
      // ...
      // 🪄 poof, the ZST is on the stack now.
      break;
  }
}

function computeAbi(ty: TyFn): FnAbi {
  const scalar = (type: wasm.ValType): ArgRetAbi =>
    ({ kind: "scalar", type } as const);
  const zst: ArgRetAbi = { kind: "zst" };

  function paramAbi(param: Ty): ArgRetAbi {
    switch (param.kind) {
      case "string":
        return STRING_ABI;
      case "fn":
        todo("fn abi");
      case "int":
        return scalar("i64");
      case "bool":
        return scalar("i32");
      case "list":
        todo("list abi");
      case "tuple":
        if (param.elems.length === 0) {
          return zst;
        } else if (param.elems.length === 1) {
          return paramAbi(param.elems[0]);
        }
        todo("complex tuple abi");
      case "struct":
        todo("struct ABI");
      case "never":
        return zst;
      case "var":
        varUnreachable();
    }
  }

  const params = ty.params.map(paramAbi);

  let ret: ArgRetAbi;
  switch (ty.returnTy.kind) {
    case "string":
      ret = STRING_ABI;
      break;
    case "fn":
      todo("fn abi");
    case "int":
      ret = scalar("i64");
      break;
    case "bool":
      ret = scalar("i32");
      break;
    case "list":
      todo("list abi");
    case "tuple":
      if (ty.returnTy.elems.length === 0) {
        ret = zst;
        break;
      } else if (ty.returnTy.elems.length === 1) {
        ret = paramAbi(ty.returnTy.elems[0]);
        break;
      }
      todo("complex tuple abi");
    case "struct":
      todo("struct ABI");
    case "never":
      ret = zst;
      break;
    case "var":
      varUnreachable();
  }

  return { params, ret };
}

function wasmTypeForAbi(abi: FnAbi): {
  type: wasm.FuncType;
  paramLocations: VarLocation[];
} {
  const params: wasm.ValType[] = [];
  const paramLocations: VarLocation[] = [];

  abi.params.forEach((arg) => {
    switch (arg.kind) {
      case "scalar":
        paramLocations.push({ kind: "local", idx: params.length });
        params.push(arg.type);
        break;
      case "zst":
        paramLocations.push({ kind: "zst" });
        break;
      case "aggregate":
        paramLocations.push({ kind: "local", idx: params.length });
        params.push(...arg.types);
        break;
    }
  });
  let returns: wasm.ValType[];
  switch (abi.ret.kind) {
    case "scalar":
      returns = [abi.ret.type];
      break;
    case "zst":
      returns = [];
      break;
    case "aggregate":
      returns = abi.ret.types;
  }

  return { type: { params, returns }, paramLocations };
}

function wasmTypeForBody(ty: Ty): wasm.ValType[] {
  switch (ty.kind) {
    case "string":
      return STRING_TYPES;
    case "int":
      return ["i64"];
    case "bool":
      return ["i32"];
    case "list":
      todo("list types");
    case "tuple":
      if (ty.elems.length === 0) {
        return [];
      } else if (ty.elems.length === 1) {
        return wasmTypeForBody(ty.elems[0]);
      }
      todo("complex tuples");
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
    _name: "___print",
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
    JSON.stringify({ kind: "builtin", name: "print" }),
    printIdx
  );

  mod.exports.push({
    name: "_start",
    desc: { kind: "func", idx: startIdx + mod.imports.length },
  });
}
