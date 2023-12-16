import {
  Pkg,
  Expr,
  ExprBlock,
  Folder,
  ItemFunction,
  ItemGlobal,
  ItemImport,
  Item,
  ItemId,
  LoopId,
  Resolution,
  Typecked,
  mkDefaultFolder,
  superFoldExpr,
  superFoldItem,
  varUnreachable,
  paramUnreachable as codegenUnreachableTy,
} from "./ast";
import { GlobalContext } from "./context";
import { unreachable } from "./error";
import { printTy } from "./printer";
import {
  Ty,
  TyFn,
  TyRawPtr,
  TyStruct,
  TyTuple,
  structFieldsSubstituted,
} from "./types";
import { ComplexMap, encodeUtf8, unwrap } from "./utils";
import * as wasm from "./wasm/defs";

const USIZE: wasm.ValType = "i32";
// POINTERS ARE JUST INTEGERS
const POINTER: wasm.ValType = USIZE;

const STRING_TYPES: wasm.ValType[] = [POINTER, USIZE];
const STRING_ABI: ArgRetAbi = STRING_TYPES;

const WASM_PAGE = 65536;

const DUMMY_IDX = 9999999;

const ALLOCATE: string[] = ["std", "rt", "alloc", "allocate"];
const DEALLOCATE: string[] = ["std", "rt", "alloc", "deallocate"];

type RelocationKind =
  | {
      kind: "funccall";
      instr: wasm.Instr & { func: wasm.FuncIdx };
    }
  | {
      kind: "globalref";
      instr: wasm.Instr & { imm: wasm.GlobalIdx };
    };

type Relocation = RelocationKind & { res: Resolution };

type FuncOrImport =
  | { kind: "func"; idx: wasm.FuncIdx }
  | { kind: "import"; idx: number };

export type Context = {
  mod: wasm.Module;
  funcTypes: ComplexMap<wasm.FuncType, wasm.TypeIdx>;
  reservedHeapMemoryStart: number;
  funcIndices: ComplexMap<Resolution, FuncOrImport>;
  globalIndices: ComplexMap<Resolution, wasm.GlobalIdx>;
  gcx: GlobalContext;
  relocations: Relocation[];
  knownDefPaths: ComplexMap<string[], ItemId>;
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
        offset: [{ kind: "i32.const", imm: 0n }],
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

const KNOWN_DEF_PATHS = [ALLOCATE, DEALLOCATE];

function getKnownDefPaths(pkgs: Pkg<Typecked>[]): ComplexMap<string[], ItemId> {
  const knows = new ComplexMap<string[], ItemId>();

  const folder: Folder<Typecked, Typecked> = {
    ...mkDefaultFolder(),
    itemInner(item): Item<Typecked> {
      KNOWN_DEF_PATHS.forEach((path) => {
        if (JSON.stringify(path) === JSON.stringify(item.defPath)) {
          knows.set(path, item.id);
        }
      });

      return superFoldItem(item, this);
    },
    expr(expr) {
      return superFoldExpr(expr, this);
    },
    ident(ident) {
      return ident;
    },
    type(type) {
      return type;
    },
  };

  pkgs.forEach((pkg) => pkg.rootItems.forEach((item) => folder.item(item)));

  return knows;
}

export function lower(gcx: GlobalContext): wasm.Module {
  const knownDefPaths = getKnownDefPaths(gcx.finalizedPkgs);

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
    gcx,
    mod,
    funcTypes: new ComplexMap(),
    funcIndices: new ComplexMap(),
    globalIndices: new ComplexMap(),
    reservedHeapMemoryStart: 0,
    relocations: [],
    knownDefPaths,
  };

  function lowerMod(items: Item<Typecked>[]) {
    items.forEach((item) => {
      switch (item.kind) {
        case "function": {
          lowerFunc(cx, item);
          break;
        }
        case "import": {
          lowerImport(cx, item);
          break;
        }
        case "mod": {
          lowerMod(item.contents);
          break;
        }
        case "global": {
          lowerGlobal(cx, item);
          break;
        }
        case "extern":
        case "type":
          break;
        case "error":
          unreachable("codegen should never see errors");
        default: {
          const _: never = item;
        }
      }
    });
  }
  gcx.finalizedPkgs.forEach((ast) => lowerMod(ast.rootItems));

  const HEAP_ALIGN = 0x08;
  cx.reservedHeapMemoryStart =
    mod.datas.length > 0
      ? (mod.datas[0].init.length + (HEAP_ALIGN - 1)) & ~(HEAP_ALIGN - 1)
      : 0;

  addRt(cx, gcx.finalizedPkgs);

  // THE LINKER
  const offset = cx.mod.imports.length;
  cx.relocations.forEach((rel) => {
    switch (rel.kind) {
      case "funccall": {
        const idx = cx.funcIndices.get(rel.res);
        if (idx === undefined) {
          unreachable(
            `no function found for relocation '${JSON.stringify(rel.res)}'`,
          );
        }
        rel.instr.func = idx.kind === "func" ? offset + idx.idx : idx.idx;
        break;
      }
      case "globalref": {
        const idx = cx.globalIndices.get(rel.res);
        if (idx === undefined) {
          unreachable(
            `no global found for relocation '${JSON.stringify(rel.res)}'`,
          );
        }
        rel.instr.imm = idx;
        break;
      }
      default: {
        const _: never = rel;
      }
    }
  });
  // END OF THE LINKER

  return mod;
}

function lowerImport(cx: Context, def: ItemImport<Typecked>) {
  const existing = cx.mod.imports.findIndex(
    (imp) => imp.module === def.module.value && imp.name === def.func.value,
  );

  let idx;
  if (existing !== -1) {
    idx = existing;
  } else {
    const abi = computeAbi(def.ty!);
    const { type: wasmType } = wasmTypeForAbi(abi, def.ty!);
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

  cx.funcIndices.set({ kind: "item", id: def.id }, { kind: "import", idx });
}

function lowerGlobal(cx: Context, def: ItemGlobal<Typecked>) {
  const globalIdx = cx.mod.globals.length;

  let valtype: "i32" | "i64";
  switch (def.init.ty.kind) {
    case "i32":
      valtype = "i32";
      break;
    case "int":
      valtype = "i64";
      break;
    default:
      unreachable(`invalid global ty: ${printTy(def.init.ty)}`);
  }

  if (def.init.kind !== "literal" || def.init.value.kind !== "int") {
    unreachable(`invalid global init: ${JSON.stringify(def)}`);
  }

  const init: wasm.Instr = {
    kind: `${valtype}.const`,
    imm: BigInt(def.init.value.value),
  };

  cx.mod.globals.push({
    _name: mangleDefPath(def.defPath),
    type: { type: valtype, mut: "var" },
    init: [init],
  });

  cx.globalIndices.set({ kind: "item", id: def.id }, globalIdx);
}

type FuncContext = {
  cx: Context;
  func: ItemFunction<Typecked>;
  wasmType: wasm.FuncType;
  wasm: wasm.Func;
  varLocations: VarLocation[];
  loopDepths: Map<LoopId, number>;
  currentBlockDepth: number;
  scratchLocals: Map<wasm.ValType, wasm.LocalIdx[]>;
};

type FnAbi = { params: ArgRetAbi[]; ret: ArgRetAbi };

type ArgRetAbi = wasm.ValType[];

type VarLocation = { localIdx: number; types: wasm.ValType[]; ty: Ty };

type StructFieldLayout = {
  types: MemoryLayoutPart[];
  ty: Ty;
};

type MemoryLayoutPart = {
  offset: number;
  type: wasm.ValType;
};

type StructLayout = {
  size: number;
  align: number;
  fields: StructFieldLayout[];
};

function lowerFunc(cx: Context, func: ItemFunction<Typecked>) {
  const abi = computeAbi(func.ty!);
  const { type: wasmType, paramLocations } = wasmTypeForAbi(abi, func.ty!);
  const type = internFuncType(cx, wasmType);

  const wasmFunc: wasm.Func = {
    _name: mangleDefPath(func.defPath),
    type,
    locals: [],
    body: [],
  };

  const fcx: FuncContext = {
    cx,
    func,
    wasmType,
    wasm: wasmFunc,
    varLocations: paramLocations,
    loopDepths: new Map(),
    currentBlockDepth: 0,
    scratchLocals: new Map(),
  };

  const body = fcx.func.body;
  if (body.kind === "asm") {
    fcx.wasm.locals = body.locals;
    fcx.wasm.body = body.instructions;
  } else {
    lowerExpr(fcx, wasmFunc.body, body);
    paramLocations.forEach((local) => {
      const refcount = needsRefcount(local.ty);
      if (refcount !== undefined) {
        // TODO: correctly deal with tuples
        loadVariable(wasmFunc.body, local);
        subRefcount(fcx, wasmFunc.body, refcount);
      }
    });
  }

  const idx = fcx.cx.mod.funcs.length;
  fcx.cx.mod.funcs.push(wasmFunc);

  fcx.cx.funcIndices.set(
    { kind: "item", id: fcx.func.id },
    { kind: "func", idx },
  );
}

/*
Expression lowering.
- the result of an expression evaluation is stored on the top of the stack
*/

type LValue =
  | {
      // An assignment to a local will be simple local.set.
      // Example: `a = 0`, `a = (0, 1)`
      // Nothing will be on the stack, the loc is here.
      kind: "fullLocal";
      loc: VarLocation;
    }
  | {
      // An assignment to a global will be global.set.
      // Example: `AAA = 0`
      // Nothing will be on the stack, the id is here.
      kind: "global";
      res: Resolution;
    }
  | {
      // Writes to a tuple fields of locals will always be local.set.
      // Example: `a.0 = 1`, `a.1.2.3 = 123`.
      // Nothing will be on the stack, the loc and offset+size are here.
      kind: "localTupleField";
      loc: VarLocation;
      offset: number;
      size: number;
    }
  | {
      // Writes to struct or tuple fields in memory will always be memory stores.
      // Example: `a.a = 0`, `a.0.b = 5`, `a.b.c.0.1 = 14`.
      // A pointer to the base will be on the stack, the offset is here.
      kind: "memoryField";
      parts: MemoryLayoutPart[];
    };

/**
 * Tries to lower an expression as an lvalue. If it succeeds, the lvalue value
 * is left on the stack while the lvalue kind is returned.
 *
 * If the expression is not an lvalue, don't do anything and return undefined.
 */
function tryLowerLValue(
  fcx: FuncContext,
  instrs: wasm.Instr[],
  expr: Expr<Typecked>,
): LValue | undefined {
  const fieldParts = (
    ty: TyStruct | TyRawPtr,
    fieldIdx: number,
  ): MemoryLayoutPart[] => unwrap(layoutOfStruct(ty).fields[fieldIdx]).types;

  switch (expr.kind) {
    case "ident":
    case "path": {
      const { res } = expr.value;

      switch (res.kind) {
        case "local": {
          return {
            kind: "fullLocal",
            loc: fcx.varLocations[fcx.varLocations.length - 1 - res.index],
          };
        }
        case "item": {
          const item = fcx.cx.gcx.findItem(res.id);
          if (item.kind !== "global") {
            unreachable("cannot store to non-global item");
          }

          return {
            kind: "global",
            res,
          };
        }
        case "builtin": {
          unreachable("cannot store to builtin");
        }
      }

      break;
    }
    case "fieldAccess": {
      // Field access lvalues (or rather, lvalues in general) are made of two important parts:
      // the _final place_, and the base.
      // `a.0.b` -> `.b` is the final place, `a.0` is the base.
      // `a.0.1` -> `a.0.1` is the final place with no base.
      // We go from the outside in. Tuple fields are a projection and therefore part of the
      // final place. A struct field means that everything left of the field access is the base.

      // The base can be codegened like a normal expression (except without reference count changes).
      // Only the final place needs special handling.
      switch (expr.lhs.ty.kind) {
        case "tuple": {
          // _potentially_ a localTupleField
          todo("tuple access");
        }
        case "struct":
        case "rawptr": {
          // Codegen the base, this leaves us with the base pointer on the stack.
          // Do not increment the refcount for an lvalue base.
          lowerExpr(fcx, instrs, expr.lhs, true);
          return {
            kind: "memoryField",
            parts: fieldParts(expr.lhs.ty, expr.field.fieldIdx!),
          };
        }
        default:
          unreachable("invalid field access lhs ty");
      }
    }
    default: {
      return undefined;
    }
  }
}

function lowerExpr(
  fcx: FuncContext,
  instrs: wasm.Instr[],
  expr: Expr<Typecked>,
  // Note: This is only forwarded through field accesses.
  noRefcountIfLvalue = false,
) {
  const ty = expr.ty;

  exprKind: switch (expr.kind) {
    case "empty": {
      // A ZST, do nothing.
      return;
    }
    case "let": {
      lowerExpr(fcx, instrs, expr.rhs);
      const types = wasmTypeForBody(expr.rhs.ty);

      const local = fcx.wasm.locals.length + fcx.wasmType.params.length;

      fcx.wasm.locals.push(...types);

      types.forEach((_, i) => {
        instrs.push({ kind: "local.set", imm: local + i });
      });

      fcx.varLocations.push({ localIdx: local, types, ty: expr.rhs.ty });

      break;
    }
    case "assign": {
      lowerExpr(fcx, instrs, expr.rhs);

      const lvalue = unwrap(
        tryLowerLValue(fcx, instrs, expr.lhs),
        "invalid assign lhs",
      );

      switch (lvalue.kind) {
        case "fullLocal": {
          storeVariable(instrs, lvalue.loc);
          break;
        }
        case "global": {
          const instr: wasm.Instr = { kind: "global.set", imm: DUMMY_IDX };
          const rel: Relocation = { kind: "globalref", instr, res: lvalue.res };

          fcx.cx.relocations.push(rel);
          instrs.push(instr);
          break;
        }
        case "localTupleField":
          todo("local tuple fields");
        case "memoryField": {
          storeMemory(fcx, instrs, lvalue.parts);
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
          instrs: lowerExprBlockBody(fcx, expr, prevVarLocationLengths),
          type: blockTypeForBody(fcx.cx, expr.ty),
        };

        instrs.push(instr);
      }

      fcx.varLocations.length = prevVarLocationLengths;
      break;
    }
    case "literal": {
      switch (expr.value.kind) {
        case "str": {
          const utf8 = encodeUtf8(expr.value.value);
          const idx = appendData(fcx.cx, utf8);

          instrs.push({ kind: "i32.const", imm: BigInt(idx) });
          instrs.push({ kind: "i32.const", imm: BigInt(utf8.length) });

          break;
        }
        case "int":
          switch (expr.value.type) {
            case "Int":
              instrs.push({ kind: "i64.const", imm: BigInt(expr.value.value) });
              break;
            case "I32":
              instrs.push({ kind: "i32.const", imm: BigInt(expr.value.value) });
              break;
          }
          break;
      }
      break;
    }
    case "path":
    case "ident": {
      const { res } = expr.value;

      switch (res.kind) {
        case "local": {
          const location =
            fcx.varLocations[fcx.varLocations.length - 1 - res.index];
          loadVariable(instrs, location);

          const refcount = needsRefcount(expr.ty);

          if (!noRefcountIfLvalue && refcount !== undefined) {
            addRefcount(
              fcx,
              instrs,
              refcount === "string" ? "string" : "struct",
            );
          }

          break;
        }
        case "item": {
          const item = fcx.cx.gcx.findItem(res.id);
          switch (item.kind) {
            case "global": {
              const instr: wasm.Instr = { kind: "global.get", imm: DUMMY_IDX };
              const rel: Relocation = { kind: "globalref", instr, res };
              instrs.push(instr);
              fcx.cx.relocations.push(rel);
              break;
            }
            default: {
              todo("non-global item ident res");
            }
          }

          break;
        }
        case "builtin":
          switch (res.name) {
            case "false":
              instrs.push({ kind: "i32.const", imm: 0n });
              break;
            case "true":
              instrs.push({ kind: "i32.const", imm: 1n });
              break;
            case "print":
              todo("print function");
            case "__NULL":
              instrs.push({ kind: "i32.const", imm: 0n });
              break;
            default: {
              unreachable(`${res.name}#B is not a value`);
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

      const lhsTy = expr.lhs.ty;
      const rhsTy = expr.rhs.ty;
      if (
        (lhsTy.kind === "int" && rhsTy.kind === "int") ||
        (lhsTy.kind === "i32" && rhsTy.kind === "i32") ||
        (lhsTy.kind === "rawptr" && rhsTy.kind === "rawptr")
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
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
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
            unreachable(`Invalid bool binary expr: ${expr.binaryKind}`);
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
            instrs.push({ kind: "i64.const", imm: 1n });
            instrs.push({ kind: "i64.xor" });
          } else if (ty.kind === "int") {
            // `xor RHS, -1` flips all bits.
            instrs.push({ kind: "i64.const", imm: -1n });
            instrs.push({ kind: "i64.xor" });
          } else if (ty.kind === "i32") {
            // `xor RHS, -1` flips all bits.
            instrs.push({ kind: "i32.const", imm: -1n });
            instrs.push({ kind: "i32.xor" });
          } else {
            unreachable("invalid type for !");
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

      const { res } = expr.lhs.value;
      if (res.kind === "builtin") {
        const assertArgs = (n: number) => {
          if (expr.args.length !== n)
            unreachable(
              `wrong amount of arguments for ${res.name}: expected ${n} found ${expr.args.length}`,
            );
        };
        switch (res.name) {
          case "trap": {
            assertArgs(0);
            instrs.push({ kind: "unreachable" });
            break exprKind;
          }
          case "__i32_load": {
            assertArgs(1);
            lowerExpr(fcx, instrs, expr.args[0]);
            instrs.push({ kind: "i32.load", imm: {} });
            break exprKind;
          }
          case "__i64_load": {
            assertArgs(1);
            lowerExpr(fcx, instrs, expr.args[0]);
            instrs.push({ kind: "i64.load", imm: {} });
            break exprKind;
          }
          case "__i32_store": {
            assertArgs(2);
            lowerExpr(fcx, instrs, expr.args[0]);
            lowerExpr(fcx, instrs, expr.args[1]);
            instrs.push({ kind: "i32.store", imm: {} });
            break exprKind;
          }
          case "__i64_store": {
            assertArgs(2);
            lowerExpr(fcx, instrs, expr.args[0]);
            lowerExpr(fcx, instrs, expr.args[1]);
            instrs.push({ kind: "i64.store", imm: {} });
            break exprKind;
          }
          case "__memory_size": {
            assertArgs(0);
            instrs.push({ kind: "memory.size" });
            break exprKind;
          }
          case "__memory_grow": {
            assertArgs(1);
            lowerExpr(fcx, instrs, expr.args[0]);
            instrs.push({ kind: "memory.grow" });
            break exprKind;
          }
          case "__i32_extend_to_i64_u": {
            assertArgs(1);
            lowerExpr(fcx, instrs, expr.args[0]);
            instrs.push({ kind: "i64.extend_i32_u" });
            break exprKind;
          }
          case "___transmute": {
            expr.args.map((arg) => lowerExpr(fcx, instrs, arg));
            // don't do anything
            break exprKind;
          }
        }
      }

      const callInstr: wasm.Instr = { kind: "call", func: DUMMY_IDX };
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
      lowerExpr(fcx, instrs, expr.lhs);

      switch (expr.lhs.ty.kind) {
        case "tuple": {
          // Tuples have a by-value ABI, so we can simply index.
          const lhsSize = argRetAbi(expr.lhs.ty).length;
          const resultAbi = argRetAbi(expr.ty);
          const resultSize = resultAbi.length;
          const wasmIdx = wasmTypeIdxForTupleField(
            expr.lhs.ty,
            expr.field.fieldIdx!,
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
            storeVariable(instrs, { localIdx, types: resultAbi, ty: expr.ty });

            Array(wasmIdx)
              .fill(0)
              .forEach(() => instrs.push({ kind: "drop" }));

            loadVariable(instrs, { localIdx, types: resultAbi, ty: expr.ty });
          }

          break;
        }
        case "struct":
        case "rawptr": {
          const ty = expr.lhs.ty;
          const layout = layoutOfStruct(ty);
          const field = layout.fields[expr.field.fieldIdx!];

          const ptrLocal = getScratchLocals(fcx, "i32", 1)[0];

          // We save the local for getting it later for all the field parts.
          instrs.push({
            kind: "local.set",
            imm: ptrLocal,
          });

          field.types.forEach((fieldPart) => {
            instrs.push({
              kind: "local.get",
              imm: ptrLocal,
            });
            switch (fieldPart.type) {
              case "i32":
                instrs.push({
                  kind: "i32.load",
                  imm: {
                    align: sizeOfValtype(fieldPart.type),
                    offset: fieldPart.offset,
                  },
                });
                break;
              case "i64":
                instrs.push({
                  kind: "i64.load",
                  imm: {
                    align: sizeOfValtype(fieldPart.type),
                    offset: fieldPart.offset,
                  },
                });
                break;
              default: {
                unreachable(
                  `unsupported struct content type: ${fieldPart.type}`,
                );
              }
            }
          });

          break;
        }
        default:
          unreachable("invalid field access lhs");
      }

      break;
    }
    case "if": {
      lowerExpr(fcx, instrs, expr.cond);

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
        type: blockTypeForBody(fcx.cx, expr.ty),
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
      if (expr.body.ty.kind !== "never") {
        bodyInstrs.push({
          kind: "br",
          label: /*innermost control structure, the loop*/ 0,
        });
      }
      fcx.currentBlockDepth -= 2;

      outerBlockInstrs.push({
        kind: "loop",
        instrs: bodyInstrs,
        type: blockTypeForBody(fcx.cx, expr.ty),
      });

      instrs.push({
        kind: "block",
        instrs: outerBlockInstrs,
        type: blockTypeForBody(fcx.cx, expr.ty),
      });
      fcx.loopDepths.delete(expr.loopId);

      break;
    }
    case "break": {
      const loopDepth = unwrap(fcx.loopDepths.get(expr.target!));

      instrs.push({
        kind: "br",
        label: fcx.currentBlockDepth - loopDepth - 1,
      });
      break;
    }
    case "structLiteral": {
      if (expr.ty.kind !== "struct") {
        unreachable("struct literal must have struct type");
      }
      const layout = layoutOfStruct(expr.ty);

      // std.rt.allocateItem(size, align);
      instrs.push({ kind: "i32.const", imm: BigInt(layout.size) });
      instrs.push({ kind: "i32.const", imm: BigInt(layout.align) });
      const allocate: wasm.Instr = { kind: "call", func: DUMMY_IDX };
      const allocateItemId = fcx.cx.knownDefPaths.get(ALLOCATE);
      if (!allocateItemId) {
        unreachable("std.rt.allocateItem not found");
      }
      fcx.cx.relocations.push({
        kind: "funccall",
        instr: allocate,
        res: { kind: "item", id: allocateItemId },
      });
      instrs.push(allocate);

      const ptrLocal = getScratchLocals(fcx, "i32", 1)[0];
      instrs.push({ kind: "local.tee", imm: ptrLocal });

      // Store the refcount
      instrs.push({ kind: "i32.const", imm: 1n });
      instrs.push({ kind: "i32.store", imm: { align: 4 } });

      // Now, set all fields.
      expr.fields.forEach((field, i) => {
        lowerExpr(fcx, instrs, field.expr);
        instrs.push({ kind: "local.get", imm: ptrLocal });
        storeMemory(fcx, instrs, layout.fields[i].types);
      });

      // Last, load the pointer and pass that on.
      instrs.push({ kind: "local.get", imm: ptrLocal });

      break;
    }
    case "tupleLiteral": {
      expr.fields.forEach((field) => lowerExpr(fcx, instrs, field));
      break;
    }
    case "asm": {
      unreachable("asm");
    }
    case "error":
      unreachable("codegen should never see errors");
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
  expr: ExprBlock<Typecked> & Expr<Typecked>,
  prevVarLocationLength: number,
): wasm.Instr[] {
  fcx.currentBlockDepth++;
  const instrs: wasm.Instr[] = [];

  const headExprs = expr.exprs.slice(0, -1);
  const tailExpr = expr.exprs[expr.exprs.length - 1];

  for (const inner of headExprs) {
    lowerExpr(fcx, instrs, inner);
    if (inner.ty.kind === "never") {
      // The rest of the block is unreachable, so we don't bother codegening it.
      break;
    }
    const types = wasmTypeForBody(inner.ty);

    const refcount = needsRefcount(inner.ty);
    if (refcount !== undefined) {
      subRefcount(fcx, instrs, refcount);
    } else {
      // TODO: correctly deal with tuples
      types.forEach(() => instrs.push({ kind: "drop" }));
    }
  }

  lowerExpr(fcx, instrs, tailExpr);

  const thisBlockLocals = fcx.varLocations.slice(prevVarLocationLength);

  thisBlockLocals.forEach((local) => {
    const refcount = needsRefcount(local.ty);
    if (refcount !== undefined) {
      // TODO: correctly deal with tuples
      loadVariable(instrs, local);
      subRefcount(fcx, instrs, refcount);
    }
  });

  fcx.currentBlockDepth--;

  return instrs;
}

function getScratchLocals(
  fcx: FuncContext,
  type: wasm.ValType,
  amount: number,
): wasm.LocalIdx[] {
  function addLocals(fcx: FuncContext, type: wasm.ValType[]): wasm.LocalIdx[] {
    const local = fcx.wasm.locals.length + fcx.wasmType.params.length;

    fcx.wasm.locals.push(...type);

    return type.map((_, i) => local + i);
  }

  const existing = fcx.scratchLocals.get(type);
  if (!existing) {
    const locals = addLocals(
      fcx,
      Array(amount)
        .fill(0)
        .map(() => type),
    );

    fcx.scratchLocals.set(type, locals);
    return locals;
  } else {
    const toAdd = amount - existing.length;
    if (toAdd > 0) {
      const locals = addLocals(
        fcx,
        Array(toAdd)
          .fill(0)
          .map(() => type),
      );

      existing.push(...locals);
      return existing;
    }
    return existing.slice(0, amount);
  }
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

/**
 * Generates TYPE.store instructions for memory parts.
 * STACK IN: [...types_, i32 (base pointer)]
 * STACK OUT: []
 */
function storeMemory(
  fcx: FuncContext,
  instrs: wasm.Instr[],
  types_: MemoryLayoutPart[],
) {
  const ptr = getScratchLocals(fcx, "i32", 1)[0];
  instrs.push({ kind: "local.set", imm: ptr });

  const types = [...types_];
  types.reverse();
  types.forEach(({ type, offset }) => {
    if (type === "externref" || type === "funcref") {
      unreachable("non-i32/i64 stores");
    }
    const val = getScratchLocals(fcx, type, 1)[0];
    instrs.push({ kind: "local.set", imm: val });
    instrs.push({ kind: "local.get", imm: ptr });
    instrs.push({ kind: "local.get", imm: val });
    const kind: wasm.SimpleStoreKind = `${type}.store`;
    instrs.push({
      kind,
      imm: { offset, align: sizeOfValtype(type) },
    });
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
    case "tuple":
      return param.elems.flatMap(argRetAbi);
    case "struct":
    case "rawptr":
      return ["i32"];
    case "never":
      return [];
    case "var":
    case "param":
    case "error":
      codegenUnreachableTy(param);
  }
}

function computeAbi(ty: TyFn): FnAbi {
  const params = ty.params.map(argRetAbi);
  const ret = argRetAbi(ty.returnTy);

  return { params, ret };
}

function wasmTypeForAbi(
  abi: FnAbi,
  ty: TyFn,
): {
  type: wasm.FuncType;
  paramLocations: VarLocation[];
} {
  const params: wasm.ValType[] = [];
  const paramLocations: VarLocation[] = [];

  abi.params.forEach((arg, i) => {
    paramLocations.push({
      localIdx: params.length,
      types: arg,
      ty: ty.params[i],
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
    case "tuple":
      return ty.elems.flatMap(wasmTypeForBody);
    case "fn":
      todo("fn types");
    case "struct":
    case "rawptr":
      return ["i32"];
    case "never":
      return [];
    case "var":
    case "param":
    case "error":
      codegenUnreachableTy(ty);
  }
}

function sizeOfValtype(type: wasm.ValType): number {
  switch (type) {
    case "i32":
    case "f32":
      return 4;
    case "i64":
    case "f64":
      return 8;
    case "v128":
    case "funcref":
    case "externref":
      unreachable("types not emitted");
  }
}

export function layoutOfStruct(ty_: TyStruct | TyRawPtr): StructLayout {
  const ty = ty_.kind === "struct" ? ty_ : ty_.inner;
  if (ty.kind !== "struct") {
    unreachable("must be struct");
  }
  const fieldTys = structFieldsSubstituted(ty).map(([_, ty]) => ty);
  const fieldWasmTys = fieldTys.map((field) => wasmTypeForBody(field));

  // TODO: Use the max alignment instead.
  const align = fieldWasmTys.some((field) =>
    field.some((type) => type === "i64"),
  )
    ? 8
    : 4;

  // i32 refcount is at offset 0.
  let offset = 4;

  const fields: StructFieldLayout[] = fieldWasmTys.map((field, i) => {
    const value: StructFieldLayout = {
      types: [],
      ty: fieldTys[i],
    };

    const types = field.map((type) => {
      const size = sizeOfValtype(type);

      // we don't want padding for the first field as the allocator takes care of that.
      if (offset !== 4 && size === 8 && offset % 8 !== 0) {
        // padding.
        offset += 4;
      }

      const fieldPart = {
        offset,
        type,
      };
      offset += size;
      return fieldPart;
    });

    value.types = types;

    return value;
  });

  if (align === 8 && offset % 8 !== 0) {
    offset += 4;
  }

  return {
    size: offset,
    align,
    fields,
  };
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

// Refcounts:

/*
 * Injects `refcount` expressions into the code to make sure
 * that no memory is leaked and no memory is freed too early.
 *
 * When do we need to adjust the refcount?
 *
 * When looking at reference counts, we need to distiguish between moves
 * and copies of a struct. When a struct is moved, no reference count has
 * to be changed. When it is copied, we need to increment the reference count.
 *
 * ```
 * let a = S {};
 * foo(a); // COPY
 * ```
 * ```
 * let a = identity(S {}); // MOVE
 * ```
 *
 * Due to the way the language is structured, this analysis is fairly simple:
 * Most expressions are considered moves, but identifiers like `a` are considered
 * copies. This is sound because the only way to refer to a value twice is to bind
 * it to a variable. So whenever we load a variable of type struct, we need to bump
 * the refcount.
 *
 * Then we just need to decrement all the locals (and params!) refcounts when they go
 * out of scope.
 *
 * This leaves us with the following rules:
 * - when loading an identifier, add an increment
 * - when the end of a block is reached, decrement all locals
 * - when the end of a function is reached, decrement all params
 * - when an expression value is ignored, decrement
 */

function needsRefcount(ty: Ty): StructLayout | "string" | undefined {
  switch (ty.kind) {
    case "string":
      // TODO: deal with strings
      return undefined;
    case "struct":
      return layoutOfStruct(ty);
    case "var":
      varUnreachable();
    default:
      return undefined;
  }
}

function addRefcount(
  fcx: FuncContext,
  instrs: wasm.Instr[],
  kind: "struct" | "string",
) {
  const layout: wasm.ValType[] = kind === "string" ? ["i32", "i32"] : ["i32"];

  const [ptr, len] = getScratchLocals(fcx, "i32", layout.length);

  // stack: PTR, {LEN}
  const innerInstrs: wasm.Instr[] = [];

  if (kind === "string") {
    innerInstrs.push({ kind: "local.set", imm: len }); // stack: PTR
  }

  innerInstrs.push({ kind: "local.tee", imm: ptr }); // stack: PTR
  innerInstrs.push({ kind: "local.get", imm: ptr }); // stack: PTR, PTR
  innerInstrs.push({ kind: "local.get", imm: ptr }); // stack: PTR, PTR, PTR
  innerInstrs.push({ kind: "i32.load", imm: { align: 4 } }); // stack: PTR, PTR, cnt
  innerInstrs.push({ kind: "i32.const", imm: 1n }); // stack: PTR, PTR, cnt, 1
  innerInstrs.push({ kind: "i32.add" }); // stack: PTR, PTR, cnt
  innerInstrs.push({ kind: "i32.store", imm: { align: 4 } }); // stack: PTR

  if (kind === "string") {
    innerInstrs.push({ kind: "local.get", imm: len }); // stack: PTR, {LEN}
  }
  // stack: PTR, {LEN}

  instrs.push({
    kind: "block",
    instrs: innerInstrs,
    type: {
      kind: "typeidx",
      idx: internFuncType(fcx.cx, { params: layout, returns: layout }),
    },
  });
}

function subRefcount(
  fcx: FuncContext,
  instrs: wasm.Instr[],
  kind: StructLayout | "string",
) {
  const deallocateItemId = fcx.cx.knownDefPaths.get(DEALLOCATE);
  if (!deallocateItemId) {
    unreachable("std.rt.deallocateItem not found");
  }

  const layout: wasm.ValType[] = kind === "string" ? ["i32", "i32"] : ["i32"];

  const [ptr, len] = getScratchLocals(fcx, "i32", layout.length);
  const count = ptr;

  const innerInstrs: wasm.Instr[] = [];

  // stack: PTR, {LEN}
  if (kind === "string") {
    innerInstrs.push({ kind: "local.set", imm: len }); // stack: PTR
  }
  innerInstrs.push({ kind: "local.tee", imm: ptr }); // stack: PTR
  innerInstrs.push({ kind: "local.get", imm: ptr }); // stack: PTR, PTR
  innerInstrs.push({ kind: "i32.load", imm: { align: 4 } }); // stack: PTR, cnt
  innerInstrs.push({ kind: "i32.const", imm: 1n }); // stack: PTR, cnt, 1
  innerInstrs.push({ kind: "i32.sub" }); // stack: PTR, cnt
  innerInstrs.push({ kind: "local.tee", imm: count }); // stack: PTR, cnt
  innerInstrs.push({
    kind: "if",
    then: [
      // stack: PTR
      { kind: "local.get", imm: count }, // stack: PTR, cnt
      { kind: "i32.store", imm: { align: 4 } }, // stack:
    ],
    else: (() => {
      // stack: PTR
      const instrs: wasm.Instr[] = [];

      if (kind === "string") {
        instrs.push({ kind: "local.get", imm: len }); // stack: PTR, len
      } else {
        instrs.push({ kind: "i32.const", imm: BigInt(kind.size) }); // stack: PTR, len
      }

      const deallocateCall: wasm.Instr = { kind: "call", func: DUMMY_IDX };
      fcx.cx.relocations.push({
        kind: "funccall",
        instr: deallocateCall,
        res: { kind: "item", id: deallocateItemId },
      });
      instrs.push(deallocateCall); // stack:

      return instrs;
    })(),
    type: {
      kind: "typeidx",
      idx: internFuncType(fcx.cx, { params: ["i32"], returns: [] }),
    },
  });

  instrs.push({
    kind: "block",
    instrs: innerInstrs,
    type: {
      kind: "typeidx",
      idx: internFuncType(fcx.cx, { params: layout, returns: [] }),
    },
  });

  // stack:
}

function todo(msg: string): never {
  unreachable(`TODO: ${msg}`);
}

// Make the program runnable using wasi-preview-1
function addRt(cx: Context, pkgs: Pkg<Typecked>[]) {
  const { mod } = cx;

  const pkg0 = unwrap(pkgs.find((pkg) => pkg.id === 0));

  const mainCall: wasm.Instr = { kind: "call", func: DUMMY_IDX };
  cx.relocations.push({
    kind: "funccall",
    instr: mainCall,
    res: unwrap(pkg0.typeckResults.main),
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
      { kind: "i32.const", imm: BigInt(iovecArray) },
      { kind: "local.get", imm: 0 },
      { kind: "i32.store", imm: { offset: 0, align: 4 } },
      // get the length and store it in the iovec
      { kind: "i32.const", imm: BigInt(iovecArray + 4) },
      { kind: "local.get", imm: 1 },
      { kind: "i32.store", imm: { offset: 0, align: 4 } },
      // now call stuff
      { kind: "i32.const", imm: /*stdout*/ 1n },
      { kind: "i32.const", imm: BigInt(iovecArray) },
      { kind: "i32.const", imm: /*iovec len*/ 1n },
      { kind: "i32.const", imm: /*out ptr*/ BigInt(printReturnValue) },
      { kind: "call", func: 0 },
      { kind: "drop" },
    ],
  };
  const printIdx = cx.mod.funcs.length;
  cx.mod.funcs.push(print);

  cx.funcIndices.set(
    { kind: "builtin", name: "print" },
    { kind: "func", idx: printIdx },
  );

  mod.exports.push({
    name: "_start",
    desc: { kind: "func", idx: startIdx + mod.imports.length },
  });
}
