import { Ast, Expr, FunctionDef, Item, Ty, TyFn, varUnreachable } from "./ast";
import * as wasm from "./wasm/defs";

type StringifiedForMap<T> = string;

type Context = {
  mod: wasm.Module;
  funcTypes: Map<StringifiedForMap<wasm.FuncType>, wasm.TypeIdx>;
  funcIndices: Map<number, wasm.FuncIdx>;
};

function internFuncType(cx: Context, type: wasm.FuncType): wasm.TypeIdx {
  const s = JSON.stringify(type);
  const existing = cx.funcTypes.get(s);
  if (existing !== undefined) {
    return existing;
  }
  const idx = cx.mod.types.length;
  cx.mod.types.push(type);
  cx.funcTypes.set(s, idx);
  return idx;
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

  const cx: Context = { mod, funcTypes: new Map(), funcIndices: new Map() };

  ast.forEach((item) => {
    switch (item.kind) {
      case "function": {
        lowerFunc(cx, item, item.node);
      }
    }
  });

  return mod;
}

type FuncContext = {
  cx: Context;
  item: Item;
  func: FunctionDef;
  wasm: wasm.Func;
  varLocations: VarLocation[];
};

type Abi = { params: ArgAbi[]; ret: RetAbi };

type ArgAbi = { kind: "scalar"; type: wasm.ValType } | { kind: "zst" };
type RetAbi = { kind: "scalar"; type: wasm.ValType } | { kind: "zst" };

type VarLocation = { kind: "local"; idx: number } | { kind: "zst" };

function lowerFunc(cx: Context, item: Item, func: FunctionDef) {
  const abi = computeAbi(func.ty!);
  const { type: wasmType, paramLocations } = wasmTypeForAbi(abi);
  const type = internFuncType(cx, wasmType);

  const wasmFunc: wasm.Func = {
    _name: func.name,
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
  fcx.cx.funcIndices.set(fcx.item.id, idx);
}

/*
Expression lowering.
- the result of an expression evaluation is stored on the top of the stack
*/

function lowerExpr(fcx: FuncContext, instrs: wasm.Instr[], expr: Expr) {
  const ty = expr.ty!;

  switch (expr.kind) {
    case "empty":
      // A ZST, do nothing.
      return;
    case "let":
      // Let, that's complicated.
      todo("let");
    case "block":
      if (expr.exprs.length === 1) {
        lowerExpr(fcx, instrs, expr.exprs[0]);
      }
      break;
    case "literal":
      switch (expr.value.kind) {
        case "str":
          todo("strings");
        case "int":
          instrs.push({ kind: "i64.const", imm: expr.value.value });
      }
      break;
    case "ident":
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
    case "binary":
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
    case "unary":
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
    case "call":
      todo("call");
    case "if":
      todo("ifs");
  }
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

function computeAbi(ty: TyFn): Abi {
  const scalar = (type: wasm.ValType): ArgAbi & RetAbi =>
    ({ kind: "scalar", type } as const);
  const zst: ArgAbi & RetAbi = { kind: "zst" };

  function paramAbi(param: Ty): ArgAbi {
    switch (param.kind) {
      case "string":
        todo("string abi");
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
      case "var":
        varUnreachable();
    }
  }

  const params = ty.params.map(paramAbi);

  let ret: RetAbi;
  switch (ty.returnTy.kind) {
    case "string":
      todo("string abi");
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
    case "var":
      varUnreachable();
  }

  return { params, ret };
}

function wasmTypeForAbi(abi: Abi): {
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
        return undefined;
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
  }

  return { type: { params, returns }, paramLocations };
}

function wasmTypeForBody(ty: Ty): wasm.ValType | undefined {
  switch (ty.kind) {
    case "string":
      todo("string types");
    case "int":
      return "i64";
    case "bool":
      return "i32";
    case "list":
      todo("list types");
    case "tuple":
      if (ty.elems.length === 0) {
        return undefined;
      } else if (ty.elems.length === 1) {
        return wasmTypeForBody(ty.elems[0]);
      }
      todo("complex tuples");
    case "fn":
      todo("fn types");
    case "var":
      varUnreachable();
  }
}

function todo(msg: string): never {
  throw new Error(`TODO: ${msg}`);
}

function exists<T>(val: T | undefined): val is T {
  return val !== undefined;
}
