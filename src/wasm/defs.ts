// Type definitions and encoding of Wasm.
// See https://webassembly.github.io/spec/core.

// Base types.

export type u32 = number;
export type u64 = bigint;
export type f32 = number;
export type f64 = number;
export type VecByte = Uint8Array;
export type Name = string;

// types

export type Numtype = "i32" | "i64" | "f32" | "f64";

export type Vectype = "v128";

export type Reftype = "funcref" | "externref";

export type ValType = Numtype | Vectype | Reftype;
export const VALTYPES: ValType[] = [
  "i32",
  "i64",
  "f32",
  "f64",
  "v128",
  "funcref",
  "externref",
];

export type ResultType = ValType[];

export type FuncType = {
  params: ResultType;
  returns: ResultType;
};

export type Limits = {
  min: u32;
  max: u32;
};

export type MemType = Limits;

export type TableType = {
  limits: Limits;
  reftype: Reftype;
};

export type GlobalType = {
  mut: Mut;
  type: ValType;
};

export type Mut = "const" | "var";

export type Externtype =
  | {
      kind: "func";
      type: FuncType;
    }
  | {
      kind: "table";
      type: TableType;
    }
  | {
      kind: "mem";
      type: MemType;
    }
  | {
      kind: "global";
      type: GlobalType;
    };

// instructions

// Value representations of the types for the assembler
export type InstrValue = {
  name: Instr["kind"];
  immediates: ImmediateValue[] | "select" | "memarg";
};
type ImmediateValue = "i32" | "i64" | "f32" | "f64" | "refkind";

const ins = (name: Instr["kind"], immediates: InstrValue["immediates"]) => ({
  name,
  immediates,
});

// . numeric

export type BitWidth = "32" | "64";
const BIT_WIDTHS: BitWidth[] = ["32", "64"];

export type Sign = "u" | "s";
const SIGNS: Sign[] = ["u", "s"];

export type NumericInstr =
  | { kind: "i32.const"; imm: bigint }
  | { kind: "i64.const"; imm: u64 }
  | { kind: "f32.const"; imm: f32 }
  | { kind: "f64.const"; imm: f64 }
  | { kind: `i${BitWidth}.${IUnOp}` }
  | { kind: `f${BitWidth}.${FUnOp}` }
  | { kind: `i${BitWidth}.${IBinOp}` }
  | { kind: `f${BitWidth}.${FBinOp}` }
  | { kind: `i${BitWidth}.${ITestOp}` }
  | { kind: `i${BitWidth}.${IRelOp}` }
  | { kind: `f${BitWidth}.${FRelOp}` }
  | { kind: `i${BitWidth}.extend8_s` }
  | { kind: `i${BitWidth}.extend16_s` }
  | { kind: "i64.extend32_s" }
  | { kind: "i32.wrap_i64" }
  | { kind: `i64.extend_i32_${Sign}` }
  | { kind: `i${BitWidth}.trunc_f${BitWidth}_${Sign}` }
  | { kind: `i${BitWidth}.trunc_sat_f${BitWidth}_${Sign}` }
  | { kind: "f32.demote_f64" }
  | { kind: "f64.promote_f32" }
  | { kind: `f${BitWidth}.convert_i${BitWidth}_${Sign}` }
  | { kind: "i32.reinterpret_f32" | "i64.reinterpret_f64" }
  | { kind: "f32.reinterpret_i32" | "f64.reinterpret_i64" };

export type IUnOp = "clz" | "ctz" | "popcnt";
const I_UN_OPS: IUnOp[] = ["clz", "ctz", "popcnt"];

export type IBinOp =
  | "add"
  | "sub"
  | "mul"
  | `div_${Sign}`
  | `rem_${Sign}`
  | "and"
  | "or"
  | "xor"
  | "shl"
  | `shr_${Sign}`
  | "rotl"
  | "rotr";
const I_BIN_OPS: IBinOp[] = [
  "add",
  "sub",
  "mul",
  "and",
  "or",
  "xor",
  "shl",
  "rotl",
  "rotr",
  ...SIGNS.flatMap((sign): IBinOp[] => [
    `div_${sign}`,
    `rem_${sign}`,
    `shr_${sign}`,
  ]),
];

const F_UN_OPS = [
  "abs",
  "neg",
  "sqrt",
  "ceil",
  "floor",
  "trunc",
  "nearest",
] as const;
export type FUnOp = (typeof F_UN_OPS)[number];

const F_BIN_OPS = [
  "add",
  "sub",
  "mul",
  "div",
  "min",
  "max",
  "copysign",
] as const;
export type FBinOp = (typeof F_BIN_OPS)[number];

const I_TEST_OPS = ["eqz"] as const;
export type ITestOp = (typeof I_TEST_OPS)[number];

export type IRelOp =
  | "eq"
  | "ne"
  | `lt_${Sign}`
  | `gt_${Sign}`
  | `le_${Sign}`
  | `ge_${Sign}`;
const I_REL_OPS: IRelOp[] = [
  "eq",
  "ne",
  ...SIGNS.flatMap((sign): IRelOp[] => [
    `lt_${sign}`,
    `gt_${sign}`,
    `le_${sign}`,
    `ge_${sign}`,
  ]),
];

const F_REL_OPS = ["eq", "ne", "lt", "gt", "le", "ge"] as const;
export type FRelOp = (typeof F_REL_OPS)[number];

const NO_IMM_NUMERIC_INSTRS: NumericInstr["kind"][] = [
  ...BIT_WIDTHS.flatMap((bitWidth): NumericInstr["kind"][] => [
    ...I_UN_OPS.map((op): NumericInstr["kind"] => `i${bitWidth}.${op}`),
    ...F_UN_OPS.map((op): NumericInstr["kind"] => `f${bitWidth}.${op}`),
    ...I_BIN_OPS.map((op): NumericInstr["kind"] => `i${bitWidth}.${op}`),
    ...F_BIN_OPS.map((op): NumericInstr["kind"] => `f${bitWidth}.${op}`),
    ...I_TEST_OPS.map((op): NumericInstr["kind"] => `i${bitWidth}.${op}`),
    ...I_REL_OPS.map((op): NumericInstr["kind"] => `i${bitWidth}.${op}`),
    ...F_REL_OPS.map((op): NumericInstr["kind"] => `f${bitWidth}.${op}`),
    `i${bitWidth}.extend8_s`,
    `i${bitWidth}.extend16_s`,
    ...BIT_WIDTHS.flatMap((bitWidth2): NumericInstr["kind"][] =>
      SIGNS.flatMap((sign): NumericInstr["kind"][] => [
        `i${bitWidth}.trunc_f${bitWidth2}_${sign}`,
        `i${bitWidth}.trunc_sat_f${bitWidth2}_${sign}`,
        `f${bitWidth}.convert_i${bitWidth2}_${sign}`,
      ]),
    ),
  ]),
  "i64.extend32_s",
  "i32.wrap_i64",
  ...SIGNS.flatMap((sign): NumericInstr["kind"] => `i64.extend_i32_${sign}`),
  "f32.demote_f64",
  "f64.promote_f32",
  "i32.reinterpret_f32",
  "i64.reinterpret_f64",
  "f32.reinterpret_i32",
  "f64.reinterpret_i64",
];
const IMM_NUMERIC_INSTRS: InstrValue[] = (
  ["i32", "i64", "f32", "f64"] as const
).map((type) => ins(`${type}.const`, [type]));

const NUMERIC_INSTRS: InstrValue[] = [
  ...NO_IMM_NUMERIC_INSTRS.map((instr) => ins(instr, [])),
  ...IMM_NUMERIC_INSTRS,
];

// . vectors

export type VectorInstr = never;
const VECTOR_INSTRS: InstrValue[] = [];

// . reference

export type ReferenceInstr =
  | { kind: "ref.null"; imm: Reftype }
  | { kind: "ref.is_null" }
  | { kind: "ref.func"; imm: FuncIdx };
const REFERENCE_INSTRS: InstrValue[] = [
  ins("ref.null", ["refkind"]),
  ins("ref.is_null", []),
  ins("ref.func", ["i32"]),
];

// . parametric

export type ParametricInstr =
  | { kind: "drop" }
  | { kind: "select"; type?: ValType[] };
const PARAMETRIC_INSTRS: InstrValue[] = [
  ins("drop", []),
  ins("select", "select"),
];

// . variable

export type VariableInstr =
  | {
      kind: `local.${"get" | "set" | "tee"}`;
      imm: LocalIdx;
    }
  | {
      kind: `global.${"get" | "set"}`;
      imm: LocalIdx;
    };
const VARIABLE_INSTR: InstrValue[] = [
  ...(["get", "set", "tee"] as const).map((kind) =>
    ins(`local.${kind}`, ["i32"]),
  ),
  ...(["get", "set"] as const).map((kind) => ins(`global.${kind}`, ["i32"])),
];

// . table

export type TableInstr =
  | {
      kind: `table.${"get" | "set" | "size" | "grow" | "fill"}`;
      imm: TableIdx;
    }
  | {
      kind: "table.copy";
      imm1: TableIdx;
      imm2: TableIdx;
    }
  | {
      kind: "table.init";
      imm1: TableIdx;
      imm2: ElemIdx;
    }
  | {
      kind: "elem.drop";
      imm: ElemIdx;
    };
const TABLE_INSTRS: InstrValue[] = [
  ...(["get", "set", "size", "grow", "fill"] as const).map((kind) =>
    ins(`table.${kind}`, ["i32"]),
  ),
  ins("table.copy", ["i32", "i32"]),
  ins("table.init", ["i32", "i32"]),
  ins("elem.drop", ["i32"]),
];

// . memory

export type MemArg = {
  offset?: u32;
  align?: u32;
};

export type SimpleStoreKind = `${`${"i" | "f"}${BitWidth}` | "v128"}.${
  | "load"
  | "store"}`;
const SIMPLE_STORES: SimpleStoreKind[] = [
  "i32.load",
  "i32.store",
  "f32.load",
  "f32.store",
  "i64.load",
  "i64.store",
  "f64.load",
  "f64.store",
  "v128.load",
  "v128.store",
];

export type MemoryInstr =
  | {
      kind: SimpleStoreKind;
      imm: MemArg;
    }
  | {
      kind: `i${BitWidth}.load${"8" | "16"}_${Sign}`;
      imm: MemArg;
    }
  | { kind: `i64.load32_${Sign}`; imm: MemArg }
  | {
      kind: `i${BitWidth}.store${"8" | "16"}`;
      imm: MemArg;
    }
  | { kind: "i64.store32"; imm: MemArg }
  | {
      kind: `memory.${"size" | "grow" | "fill" | "copy"}`;
    }
  | {
      kind: "memory.init";
      imm: DataIdx;
    }
  | { kind: "data.drop"; imm: DataIdx };
const MEMORY_INSTRS_WITH_MEMARG: MemoryInstr["kind"][] = [
  ...SIMPLE_STORES,
  "i32.load8_u",
  "i32.load8_s",
  "i32.load16_u",
  "i32.load16_s",
  "i64.load8_u",
  "i64.load8_s",
  "i64.load16_u",
  "i64.load16_s",
  //
  "i64.load32_u",
  "i64.load32_s",
  //
  "i32.store8",
  "i32.store16",
  "i64.store8",
  "i64.store16",
  "i64.store32",
];
const MEMORY_INSTRS: InstrValue[] = [
  ...MEMORY_INSTRS_WITH_MEMARG.map((kind) => ins(kind, "memarg")),
  ins("memory.size", []),
  ins("memory.grow", []),
  ins("memory.fill", []),
  ins("memory.copy", []),
  ins("memory.init", ["i32"]),
  ins("data.drop", ["i32"]),
];

// . control

export type Blocktype =
  | { kind: "typeidx"; idx: TypeIdx }
  | { kind: "valtype"; type?: ValType };

export type ControlInstr =
  | {
      kind: "nop" | "unreachable";
    }
  | {
      kind: "block" | "loop";
      type: Blocktype;
      instrs: Instr[];
    }
  | {
      kind: "if";
      type: Blocktype;
      then: Instr[];
      else: Instr[];
    }
  | {
      kind: "br" | "br_if";
      label: LabelIdx;
    }
  | {
      kind: "br_table";
      labels: LabelIdx[];
      label: LabelIdx;
    }
  | {
      kind: "return";
    }
  | {
      kind: "call";
      func: FuncIdx;
    }
  | {
      kind: "call_indirect";
      table: TableIdx;
      type: TypeIdx;
    };

// . final

export type Instr =
  | NumericInstr
  // eslint-disable-next-line @typescript-eslint/no-redundant-type-constituents
  | VectorInstr
  | ReferenceInstr
  | ParametricInstr
  | VariableInstr
  | TableInstr
  | MemoryInstr
  | ControlInstr;

export const INSTRS: InstrValue[] = [
  ...NUMERIC_INSTRS,
  ...VECTOR_INSTRS,
  ...REFERENCE_INSTRS,
  ...PARAMETRIC_INSTRS,
  ...VARIABLE_INSTR,
  ...TABLE_INSTRS,
  ...MEMORY_INSTRS,
];

export type Expr = Instr[];

// Modules

export type Module = {
  types: FuncType[];
  funcs: Func[];
  tables: Table[];
  mems: Mem[];
  globals: Global[];
  elems: Elem[];
  datas: Data[];
  start?: Start;
  imports: Import[];
  exports: Export[];
  _name?: string;
};

export type TypeIdx = u32;
export type FuncIdx = u32;
export type TableIdx = u32;
export type MemIdx = u32;
export type GlobalIdx = u32;
export type ElemIdx = u32;
export type DataIdx = u32;
export type LocalIdx = u32;
export type LabelIdx = u32;

export type Func = {
  type: TypeIdx;
  locals: ValType[];
  body: Expr;
  _name?: string;
};

export type Table = {
  type: TableType;
  _name?: string;
};

export type Mem = {
  type: MemType;
  _name?: string;
};

export type Global = {
  type: GlobalType;
  init: Expr;
  _name?: string;
};

export type Elem = unknown;

export type Data = {
  init: VecByte;
  mode: Datamode;
  _name?: string;
};

export type DatamodeActive = { kind: "active"; memory: MemIdx; offset: Expr };
export type Datamode = { kind: "passive" } | DatamodeActive;

export type Start = {
  func: FuncIdx;
};

export type Export = {
  name: Name;
  desc: ExportDesc;
};

export type ExportDesc =
  | {
      kind: "func";
      idx: FuncIdx;
    }
  | { kind: "table"; idx: TableIdx }
  | { kind: "memory"; idx: MemIdx }
  | { kind: "global"; idx: GlobalIdx };

export type Import = {
  module: Name;
  name: Name;
  desc: ImportDesc;
};

export type ImportDesc =
  | {
      kind: "func";
      type: TypeIdx;
    }
  | { kind: "table"; type: TableType }
  | { kind: "memory"; type: MemType }
  | { kind: "global"; type: GlobalType };
