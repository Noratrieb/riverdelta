// Type definitions and encoding of Wasm.
// See https://webassembly.github.io/spec/core.

// Base types.

export type Vec<T> = T[];
export type u32 = number;
export type u64 = number;
export type f32 = number;
export type f64 = number;
export type VecByte = Uint8Array;
export type Name = string;

// types

export type Numtype = "i32" | "i64" | "f32" | "f64";

export type Vectype = "v128";

export type Reftype = "funcref" | "externref";

export type ValType = Numtype | Vectype | Reftype;

export type ResultType = Vec<ValType>;

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

// . numeric

export type BitWidth = "32" | "64";

export type Sign = "u" | "s";

export type NumericInstr =
  | { kind: "i32.const"; imm: u32 }
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
  | { kind: `i64.extend32_s` }
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

export type FUnOp =
  | "abs"
  | "neg"
  | "sqrt"
  | "ceil"
  | "floor"
  | "trunc"
  | "nearest";

export type FBinOp = "add" | "sub" | "mul" | "div" | "min" | "max" | "copysign";

export type ITestOp = "eqz";

export type IRelOp =
  | "eq"
  | "ne"
  | `lt_${Sign}`
  | `gt_${Sign}`
  | `le_${Sign}`
  | `ge_${Sign}`;

export type FRelOp = "eq" | "ne" | "lt" | "gt" | "le" | "ge";

// . vectors

export type VectorInstr = never;

// . reference

export type ReferenceInstr =
  | { kind: "ref.null"; imm: Reftype }
  | { kind: "ref.is_null" }
  | { kind: "ref.func"; imm: FuncIdx };

// . parametric

export type ParametricInstr =
  | { kind: "drop" }
  | { kind: "select"; type?: ValType[] };

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

// . memory

export type MemArg = {
  offset?: u32;
  align?: u32;
};

export type MemoryInstr =
  | {
      kind: `${`${"i" | "f"}${BitWidth}` | "v128"}.${"load" | "store"}`;
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
  | VectorInstr
  | ReferenceInstr
  | ParametricInstr
  | VariableInstr
  | TableInstr
  | MemoryInstr
  | ControlInstr;

export type Expr = Instr[];

// Modules

export type Module = {
  types: Vec<FuncType>;
  funcs: Vec<Func>;
  tables: Vec<Table>;
  mems: Vec<Mem>;
  globals: Vec<Global>;
  elems: Vec<Elem>;
  datas: Vec<Data>;
  start?: Start;
  imports: Vec<Import>;
  exports: Vec<Export>;
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
  locals: Vec<ValType>;
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
