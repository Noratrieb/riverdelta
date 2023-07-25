// This module converts the Wasm definitions to the WAT
// WebAssembly text format for easier debugging and inspection.

import {
  Blocktype,
  Data,
  Elem,
  Export,
  Func,
  Functype as FuncType,
  Global,
  GlobalType,
  Import,
  Instr,
  Limits,
  Mem,
  MemArg,
  Module,
  Start,
  Table,
  TableType as TableType,
  Valtype as ValType,
} from "./defs";

const INLINE_SYM = Symbol.for("inline");
const INLINE_OWN_LINE = Symbol.for("inline_own_line");

type Sexpr = string | number | Sexpr[] | { inline: Symbol; items: Sexpr[] };

export function writeModuleWat(module: Module) {
  const sexprs = sexprModule(module);
  console.dir(sexprs, { depth: 100 });
  console.log(printSexpr(sexprs));
}

function printSexpr(sexpr: Sexpr): string {
  if (typeof sexpr === "string") {
    return sexpr;
  } else if (typeof sexpr === "number") {
    return String(sexpr);
  } else if (typeof sexpr === "object" && "inline" in sexpr) {
    return sexpr.items.map(printSexpr).join(" ");
  } else {
    const all = sexpr.map(printSexpr).join(" ");
    return `(${all})`;
  }
}

function inline(items: Sexpr[]): Sexpr {
  return { inline: INLINE_SYM, items };
}

function inlineOwnLine(items: Sexpr[]): Sexpr {
  return { inline: INLINE_OWN_LINE, items };
}

// base

function sexprString(s: string): Sexpr {
  // TODO: escaping
  return `"$${s}"`;
}

function sexprBinaryString(buf: Uint8Array): Sexpr {
  todo();
}

function optArr<T>(elem?: T): T[] {
  return elem !== undefined ? [elem] : [];
}

// types

function sexprValtype(type: ValType): Sexpr {
  return type;
}

function sexprFuncType(type: FuncType): Sexpr {
  return ["func", ["param", ...type.params], ["result", ...type.returns]];
}

function sexprLimits(limits: Limits): Sexpr {
  return inline([limits.min, limits.max]);
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

function sexprBlockType(type: Blocktype): Sexpr {
  return type.kind === "typeidx"
    ? type.idx
    : type.type
    ? sexprValtype(type.type)
    : inline([]);
}

function sexprMemarg(arg: MemArg): Sexpr {
  const align = arg.align !== undefined ? `align=${arg.align}` : "";
  const offset = arg.offset /*0->false*/ ? `offset=${arg.offset}` : "";
  return inline([offset, align]);
}

function sexprInstr(instr: Instr): Sexpr {
  switch (instr.kind) {
    case "block":
    case "loop":
      return inlineOwnLine([
        instr.kind,
        sexprBlockType(instr.type),
        ...instr.instr.map(sexprInstr),
        "end",
      ]);
    case "if":
      if (instr.else.length === 0) {
        return inlineOwnLine([
          instr.kind,
          sexprBlockType(instr.type),
          ...instr.then.map(sexprInstr),
          "end",
        ]);
      } else {
        return inlineOwnLine([
          instr.kind,
          sexprBlockType(instr.type),
          ...instr.then.map(sexprInstr),
          "else",
          ...instr.else.map(sexprInstr),
          "end",
        ]);
      }
    case "unreachable":
    case "nop":
    case "return":
    case "ref.is_null":
    case "drop":
    case "memory.size":
    case "memory.grow":
    case "memory.fill":
    case "memory.copy":
    // numeric i32
    case "i32.clz":
    case "i32.ctz":
    case "i32.popcnt":
    case "i32.add":
    case "i32.sub":
    case "i32.mul":
    case "i32.div_s":
    case "i32.div_u":
    case "i32.rem_s":
    case "i32.rem_u":
    case "i32.and":
    case "i32.or":
    case "i32.xor":
    case "i32.shl":
    case "i32.shr_s":
    case "i32.shr_u":
    case "i32.rotl":
    case "i32.rotr":
    // numeric i64
    case "i64.clz":
    case "i64.ctz":
    case "i64.popcnt":
    case "i64.add":
    case "i64.sub":
    case "i64.mul":
    case "i64.div_s":
    case "i64.div_u":
    case "i64.rem_s":
    case "i64.rem_u":
    case "i64.and":
    case "i64.or":
    case "i64.xor":
    case "i64.shl":
    case "i64.shr_s":
    case "i64.shr_u":
    case "i64.rotl":
    case "i64.rotr":
    // numeric f32
    case "f32.abs":
    case "f32.neg":
    case "f32.ceil":
    case "f32.floor":
    case "f32.trunc":
    case "f32.nearest":
    case "f32.sqrt":
    case "f32.add":
    case "f32.sub":
    case "f32.mul":
    case "f32.div":
    case "f32.min":
    case "f32.max":
    case "f32.copysign":
    // numeric f64
    case "f64.abs":
    case "f64.neg":
    case "f64.ceil":
    case "f64.floor":
    case "f64.trunc":
    case "f64.nearest":
    case "f64.sqrt":
    case "f64.add":
    case "f64.sub":
    case "f64.mul":
    case "f64.div":
    case "f64.min":
    case "f64.max":
    case "f64.copysign":
    // more numeric i32
    case "i32.eqz":
    case "i32.eq":
    case "i32.ne":
    case "i32.lt_s":
    case "i32.lt_u":
    case "i32.gt_s":
    case "i32.gt_u":
    case "i32.le_s":
    case "i32.le_u":
    case "i32.ge_s":
    case "i32.ge_u":
    // more numeric i64
    case "i64.eqz":
    case "i64.eq":
    case "i64.ne":
    case "i64.lt_s":
    case "i64.lt_u":
    case "i64.gt_s":
    case "i64.gt_u":
    case "i64.le_s":
    case "i64.le_u":
    case "i64.ge_s":
    case "i64.ge_u":
    // more numeric f32
    case "f32.eq":
    case "f32.ne":
    case "f32.lt":
    case "f32.gt":
    case "f32.le":
    case "f32.ge":
    // more numeric f64
    case "f64.eq":
    case "f64.ne":
    case "f64.lt":
    case "f64.gt":
    case "f64.le":
    case "f64.ge":
    // more numeric
    case "i32.wrap_i64":
    case "i32.trunc_f32_s":
    case "i32.trunc_f32_u":
    case "i32.trunc_f64_u":
    case "i32.trunc_f64_s":
    case "i32.trunc_sat_f32_s":
    case "i32.trunc_sat_f32_u":
    case "i32.trunc_sat_f64_u":
    case "i32.trunc_sat_f64_s":
    case "i64.extend_i32_s":
    case "i64.extend_i32_u":
    case "i64.trunc_f32_s":
    case "i64.trunc_f32_u":
    case "i64.trunc_f64_u":
    case "i64.trunc_f64_s":
    case "i64.trunc_sat_f32_s":
    case "i64.trunc_sat_f32_u":
    case "i64.trunc_sat_f64_u":
    case "i64.trunc_sat_f64_s":
    case "f32.convert_i32_s":
    case "f32.convert_i32_u":
    case "f32.convert_i64_s":
    case "f32.convert_i64_u":
    case "f32.demote_f64":
    case "f64.convert_i32_s":
    case "f64.convert_i32_u":
    case "f64.convert_i64_s":
    case "f64.convert_i64_u":
    case "f64.promote_f32":
    case "i32.reinterpret_f32":
    case "i64.reinterpret_f64":
    case "f32.reinterpret_i32":
    case "f64.reinterpret_i64":
    // int extend
    case "i32.extend8_s":
    case "i32.extend16_s":
    case "i64.extend8_s":
    case "i64.extend16_s":
    case "i64.extend32_s":
      return instr.kind;
    case "br":
    case "br_if":
      return inlineOwnLine([instr.kind, instr.label]);
    case "br_table":
      return inlineOwnLine([instr.kind, ...instr.labels, instr.label]);
    case "call":
      return inlineOwnLine([instr.kind, instr.func]);
    case "call_indirect":
      return inlineOwnLine([instr.kind, instr.table, instr.type]);
    case "i32.const":
    case "i64.const":
    case "f32.const":
    case "f64.const":
    case "ref.null":
    case "ref.func":
    case "local.get":
    case "local.set":
    case "local.tee":
    case "global.get":
    case "global.set":
    case "table.get":
    case "table.set":
    case "table.size":
    case "table.grow":
    case "table.fill":
    case "elem.drop":
    case "memory.init":
    case "data.drop":
      return inlineOwnLine([instr.kind, instr.imm]);
    case "select":
      return inlineOwnLine([
        instr.kind,
        ...optArr(instr.type?.map(sexprValtype)),
      ]);
    case "table.copy":
    case "table.init":
      return inlineOwnLine([instr.kind, instr.imm1, instr.imm2]);
    case "i32.load":
    case "i64.load":
    case "f32.load":
    case "f64.load":
    case "i32.load8_s":
    case "i32.load8_u":
    case "i32.load16_s":
    case "i32.load16_u":
    case "i64.load8_s":
    case "i64.load8_u":
    case "i64.load16_s":
    case "i64.load16_u":
    case "i64.load32_s":
    case "i64.load32_u":
    case "i32.store":
    case "i64.store":
    case "f32.store":
    case "f64.store":
    case "i32.store8":
    case "i32.store16":
    case "i64.store8":
    case "i64.store16":
    case "i64.store32":
    case "v128.load":
    case "v128.store":
      return inlineOwnLine([instr.kind, sexprMemarg(instr.imm)]);
  }
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
    ...optArr(func._name),
    ["type", func.type],
    ...func.locals.map((local) => ["local", sexprValtype(local)]),
    ...func.body.map((instr) => sexprInstr(instr)),
  ];
}

function sexprTable(table: Table): Sexpr {
  return ["table", ...optArr(table._name), sexprTableType(table.type)];
}

function sexprMem(mem: Mem): Sexpr {
  return ["memory", ...optArr(mem._name), sexprLimits(mem.type)];
}

function sexprGlobal(global: Global): Sexpr {
  return [
    "global",
    ...optArr(global._name),
    sexprGlobalType(global.type),
    ...global.init.map(sexprInstr),
  ];
}
function sexprExport(export_: Export): Sexpr {
  const desc = export_.desc;
  let exportDesc;
  switch (desc.kind) {
    case "func":
      exportDesc = ["func", desc.idx];
      break;
    case "table":
      exportDesc = ["table", desc.idx];
      break;
    case "mem":
      exportDesc = ["memory", desc.idx];
      break;
    case "global":
      exportDesc = ["global", desc.idx];
      break;
  }
  return ["export", export_.name, exportDesc];
}

function sexprStart(start: Start): Sexpr {
  return ["start", start.func];
}
function sexprElem(_elem: Elem): Sexpr {
  todo();
}
function sexprData(data: Data): Sexpr {
  let mode = data.mode;
  if (mode.kind === "passive") {
    return ["data", ...optArr(data._name), sexprBinaryString(data.init)];
  } else {
    return [
      "data",
      ...optArr(data._name),
      ...optArr(mode.memory === 0 ? undefined : ["memory", mode.memory]),
      ["offset", ...mode.offset.map(sexprInstr)],
      sexprBinaryString(data.init),
    ];
  }
}

function sexprModule(module: Module): Sexpr {
  return [
    "module",
    ...optArr(module._name),
    ...module.types.map(sexprType),
    ...module.imports.map(sexprImport),
    ...module.funcs.map(sexprFunction),
    ...module.tables.map(sexprTable),
    ...module.mems.map(sexprMem),
    ...module.globals.map(sexprGlobal),
    ...module.exports.map(sexprExport),
    ...optArr(module.start && sexprStart(module.start)),
    ...module.elems.map(sexprElem),
    ...module.datas.map(sexprData),
  ];
}

function todo(): never {
  throw new Error("todo");
}
