// This module converts the Wasm definitions to the WAT
// WebAssembly text format for easier debugging and inspection.

import {
  Func,
  Functype as FuncType,
  GlobalType,
  Import,
  Instr,
  Limits,
  Module,
  TableType as TableType,
  Valtype as ValType,
} from "./defs";

const INLINE_SYM = Symbol.for("inline");

type Sexpr = string | number | Sexpr[] | { inline: Symbol; items: Sexpr[] };

export function writeModuleWat(module: Module) {
  const sexprs = sexprModule(module);
  console.dir(sexprs, { depth: 100 });
}

// base

function sexprString(s: string): Sexpr {
  // TODO: escaping
  return `"$${s}"`;
}

// types

function sexprValtype(type: ValType): Sexpr {
  return type;
}

function sexprFuncType(type: FuncType): Sexpr {
  return ["func", ["param", ...type.params], ["result", ...type.returns]];
}

function sexprLimits(limits: Limits): Sexpr {
  return { inline: INLINE_SYM, items: [limits.min, limits.max] };
}

function sexprTableType(type: TableType): Sexpr {
  todo();
}

function sexprGlobalType(type: GlobalType): Sexpr {
  return type.mut === "const"
    ? sexprValtype(type.type)
    : ["mut", sexprValtype(type.type)];
}

// instrs

function sexprInstr(instr: Instr): Sexpr {
  todo();
}

// modules

function sexprType(type: FuncType): Sexpr {
  return ["type", sexprFuncType(type)];
}

function sexprImport(import_: Import): Sexpr {
  const desc = import_.desc;
  let importDesc: Sexpr;
  switch (desc.kind) {
    case "func":
      importDesc = ["type", desc.type];
      break;
    case "table":
      importDesc = sexprTableType(desc.type);
      break;
    case "memory":
      importDesc = sexprLimits(desc.type);
      break;
    case "global":
      importDesc = sexprGlobalType(desc.type);
      break;
  }

  return [
    "import",
    sexprString(import_.module),
    sexprString(import_.name),
    [desc.kind, importDesc],
  ];
}

function sexprFunction(func: Func): Sexpr {
  return [
    "func",
    ...(func._name ? [func._name] : []),
    ["type", func.type],
    ...func.locals.map((local) => ["local", sexprValtype(local)]),
    ...func.body.map((instr) => sexprInstr(instr)),
  ];
}

function sexprModule(module: Module): Sexpr {
  return [
    "module",
    ...(module._name ? [module._name] : []),
    ...module.types.map(sexprType),
    ...module.imports.map(sexprImport),
    ...module.funcs.map(sexprFunction),
  ];
}

function todo(): never {
  throw new Error("todo");
}
