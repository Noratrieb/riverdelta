import { Ast, FunctionDef, Item, Ty, TyFn, varUnreachable } from "./ast";
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
        const fcx: FuncContext = {
          cx,
          item,
          func: item.node,
        };

        lowerFunc(fcx);
      }
    }
  });

  return mod;
}

type FuncContext = {
  cx: Context;
  item: Item;
  func: FunctionDef;
};

type Abi = { params: ArgAbi[]; ret: RetAbi };

type ArgAbi = { kind: "scalar"; type: wasm.ValType } | { kind: "zst" };
type RetAbi = { kind: "scalar"; type: wasm.ValType } | { kind: "zst" };

function lowerFunc(fcx: FuncContext) {
  const abi = computeAbi(fcx.func.ty!);
  const wasmType = wasmTypeForAbi(abi);
  const type = internFuncType(fcx.cx, wasmType);

  const wasmFunc: wasm.Func = {
    type,
    locals: [],
    body: [],
  };

  const idx = fcx.cx.mod.funcs.length;
  fcx.cx.mod.funcs.push(wasmFunc);
  fcx.cx.funcIndices.set(fcx.item.id, idx);
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

function wasmTypeForAbi(abi: Abi): wasm.FuncType {
  const params = abi.params
    .map((arg) => {
      switch (arg.kind) {
        case "scalar":
          return arg.type;
        case "zst":
          return undefined;
      }
    })
    .filter(exists);

  let returns: wasm.ValType[];
  switch (abi.ret.kind) {
    case "scalar":
      returns = [abi.ret.type];
      break;
    case "zst":
      returns = [];
      break;
  }

  return { params, returns };
}

function todo(msg: string): never {
  throw new Error(`TODO: ${msg}`);
}

function exists<T>(val: T | undefined): val is T {
  return val !== undefined;
}
