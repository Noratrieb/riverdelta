// This module converts the Wasm definitions to the WAT
// WebAssembly text format for easier debugging and inspection.

import chalk from "chalk";
import {
  Blocktype,
  Data,
  DatamodeActive,
  Elem,
  Export,
  Func,
  FuncType,
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
  TableType,
  ValType,
} from "./defs";

const identity = (s: string) => s;

class Formatter {
  print: (chunk: string) => void;
  indentation: number;
  wordsInSexpr: number[];
  freshLinebreak: boolean;
  color: boolean;

  constructor(print: (chunk: string) => void, color = true) {
    this.print = print;
    this.indentation = 0;
    this.wordsInSexpr = [];
    this.freshLinebreak = false;
    this.color = color;
  }

  linebreak() {
    this.print("\n");
    this.print("  ".repeat(this.indentation));
    this.freshLinebreak = true;
  }
  breakIndent() {
    this.indentation++;
    this.linebreak();
  }
  breakDedent() {
    this.indentation--;
    if (this.indentation < 0) {
      throw new Error(
        "Cannot dedent from 0 indents, there are more dedents than indents"
      );
    }
    this.linebreak();
  }

  sexpr(f: () => void) {
    this.startSexpr();
    f();
    this.endSexpr();
  }

  keyword(word: string) {
    this.word(word, chalk.blue);
  }

  type(word: string | number) {
    this.word(word, chalk.green);
  }

  word(word: string | number, color: (s: string) => string = identity) {
    const last = this.wordsInSexpr.length - 1;
    if (this.wordsInSexpr[last] > 0 && !this.freshLinebreak) {
      // The first word hugs the left parenthesis.
      this.print(" ");
    }
    this.freshLinebreak = false;
    if (this.color) {
      this.print(color(String(word)));
    } else {
      this.print(String(word));
    }
    this.wordsInSexpr[last]++;
  }

  startSexpr() {
    this.word("(");
    this.wordsInSexpr.push(0);
  }
  endSexpr() {
    this.print(")");
    this.freshLinebreak = false;
    this.wordsInSexpr.pop();
  }
}

export function writeModuleWatToString(module: Module, color = false): string {
  const parts: string[] = [];
  const writer = (s: string) => parts.push(s);
  printModule(module, new Formatter(writer, color));
  return parts.join("");
}

export function writeModuleWat(module: Module, f: Formatter) {
  printModule(module, f);
}
// base

function printString(s: string, f: Formatter) {
  // TODO: escaping
  f.word(`"${s}"`);
}

function printBinaryString(buf: Uint8Array, f: Formatter) {
  f.word(`"${buf.toString()}"`);
}

function printId(id: string | undefined, f: Formatter) {
  if (id) {
    f.word(`$${id}`);
  }
}

// types

function printValType(type: ValType, f: Formatter) {
  f.type(type);
}

function printFuncType(type: FuncType, f: Formatter) {
  f.sexpr(() => {
    f.keyword("func");
    f.sexpr(() => {
      f.keyword("param");
      type.params.forEach((param) => printValType(param, f));
    });
    f.sexpr(() => {
      f.keyword("result");
      type.returns.forEach((type) => printValType(type, f));
    });
  });
}

function printLimits(limits: Limits, f: Formatter) {
  f.word(limits.min);
  f.word(limits.max);
}

function printTableType(type: TableType, f: Formatter) {
  printLimits(type.limits, f);
  printValType(type.reftype, f);
}

function printGlobalType(type: GlobalType, f: Formatter) {
  if (type.mut === "const") {
    printValType(type.type, f);
  } else {
    f.sexpr(() => {
      f.keyword("mut");
      printValType(type.type, f);
    });
  }
}

// instrs

function printBlockType(type: Blocktype, f: Formatter) {
  f.sexpr(() => {
    f.keyword("type");
    if (type.kind === "typeidx") {
      f.type(type.idx);
    } else if (type.type !== undefined) {
      printValType(type.type, f);
    }
  });
}

function printMemarg(arg: MemArg, f: Formatter) {
  if (arg.align !== undefined) {
    f.word(`align=${arg.align}`);
  }
  if (arg.offset /*0->false*/) {
    `offset=${arg.offset}`;
  }
}

/**
 * Print a list of instructions with one extra indentation.
 * Start: indented start of first instr
 * End: start of next line
 */
function printInstrBlock(instrs: Instr[], f: Formatter) {
  instrs.forEach((nested, i) => {
    printInstr(nested, f);
    if (i !== instrs.length - 1) {
      f.linebreak();
    }
  });
  f.breakDedent();
}

function printInstr(instr: Instr, f: Formatter) {
  switch (instr.kind) {
    case "block":
    case "loop":
      f.word(instr.kind);
      printBlockType(instr.type, f);
      f.breakIndent();
      printInstrBlock(instr.instrs, f);
      f.word("end");
      break;
    case "if":
      if (instr.else.length === 0) {
        f.word(instr.kind);
        printBlockType(instr.type, f);
        f.breakIndent();
        printInstrBlock(instr.then, f);
        f.word("end");
      } else {
        f.word(instr.kind);
        printBlockType(instr.type, f);
        f.breakIndent();
        printInstrBlock(instr.then, f);
        f.word("else");
        f.breakIndent();
        printInstrBlock(instr.else, f);
        f.word("end");
      }
      break;
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
      f.word(instr.kind);
      break;
    case "br":
    case "br_if":
      f.word(instr.kind);
      f.word(instr.label);
      break;
    case "br_table":
      f.word(instr.kind);
      instr.labels.forEach((label) => f.word(label));
      f.word(instr.label);
      break;
    case "call":
      f.word(instr.kind);
      f.word(instr.func);
      break;
    case "call_indirect":
      f.word(instr.kind);
      f.word(instr.table);
      f.word(instr.type);
      break;
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
      f.word(instr.kind);
      f.word(instr.imm);
      break;
    case "select":
      f.word(instr.kind);
      instr.type?.forEach((type) => printValType(type, f));
      break;
    case "table.copy":
    case "table.init":
      f.word(instr.kind);
      f.word(instr.imm1);
      f.word(instr.imm2);
      break;
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
      f.word(instr.kind);
      printMemarg(instr.imm, f);
      break;
  }
}

// modules

function printType(type: FuncType, f: Formatter) {
  f.sexpr(() => {
    f.keyword("type");
    printFuncType(type, f);
  });
}

function printImport(import_: Import, f: Formatter) {
  f.sexpr(() => {
    f.keyword("import");
    printString(import_.module, f);
    printString(import_.name, f);

    const desc = import_.desc;
    f.sexpr(() => {
      f.word(desc.kind);
      switch (desc.kind) {
        case "func":
          f.sexpr(() => {
            f.keyword("type");
            f.type(desc.type);
          });
          break;
        case "table":
          printTableType(desc.type, f);
          break;
        case "memory":
          printLimits(desc.type, f);
          break;
        case "global":
          printGlobalType(desc.type, f);
          break;
      }
    });
  });
}

function printFunction(func: Func, f: Formatter) {
  f.sexpr(() => {
    f.keyword("func");
    printId(func._name, f);

    f.sexpr(() => {
      f.keyword("type");
      f.type(func.type);
    });

    if (func.locals.length > 0 || func.body.length > 0) {
      f.breakIndent();
    }

    if (func.locals.length > 0) {
      f.sexpr(() => {
        f.keyword("local");

        func.locals.forEach((local) => printValType(local, f));
      });
      f.linebreak();
    }
    if (func.body.length > 0) {
      printInstrBlock(func.body, f);
    }
  });
}

function printTable(table: Table, f: Formatter) {
  f.sexpr(() => {
    f.keyword("table");
    printId(table._name, f);
    printTableType(table.type, f);
  });
}

function printMem(mem: Mem, f: Formatter) {
  f.sexpr(() => {
    f.keyword("memory");
    printId(mem._name, f);

    printLimits(mem.type, f);
  });
}

function printGlobal(global: Global, f: Formatter) {
  f.sexpr(() => {
    f.keyword("global");
    printId(global._name, f);

    printGlobalType(global.type, f);

    f.breakIndent();
    printInstrBlock(global.init, f);
  });
}

function printExport(export_: Export, f: Formatter) {
  const desc = export_.desc;

  f.sexpr(() => {
    f.keyword("export");
    printString(export_.name, f);

    f.sexpr(() => {
      f.word(desc.kind);
      f.word(desc.idx);
    });
  });
}

function printStart(start: Start, f: Formatter) {
  f.sexpr(() => {
    f.keyword("start");
    f.word(start.func);
  });
}

function printElem(_elem: Elem, f: Formatter) {
  todo();
}

function printData(data: Data, f: Formatter) {
  let mode = data.mode;

  f.sexpr(() => {
    f.keyword("data");
    printId(data._name, f);

    if (mode.kind === "active") {
      const active: DatamodeActive = mode;
      if (active.memory !== 0) {
        f.sexpr(() => {
          f.keyword("memory");
          f.word(active.memory);
        });
      }

      f.sexpr(() => {
        if (active.offset.length === 1) {
          printInstr(active.offset[0], f);
        } else {
          f.keyword("offset");
          f.linebreak();
          printInstrBlock(active.offset, f);
        }
      });
    }

    printBinaryString(data.init, f);
  });
}

function printModule(module: Module, f: Formatter) {
  f.sexpr(() => {
    f.keyword("module");
    printId(module._name, f);
    f.breakIndent();

    let hasAnything = false;
    const breakIfAny = () => {
      if (hasAnything) {
        f.linebreak();
      }
      hasAnything = true;
    };

    module.types.forEach((type) => {
      breakIfAny();
      printType(type, f);
    });

    module.imports.forEach((import_) => {
      breakIfAny();
      printImport(import_, f);
    });

    module.funcs.forEach((func) => {
      breakIfAny();
      printFunction(func, f);
    });

    module.tables.forEach((table) => {
      breakIfAny();
      printTable(table, f);
    });

    module.mems.forEach((mem) => {
      breakIfAny();
      printMem(mem, f);
    });

    module.globals.forEach((global) => {
      breakIfAny();
      printGlobal(global, f);
    });

    module.exports.forEach((export_) => {
      breakIfAny();
      printExport(export_, f);
    });

    if (module.start) {
      breakIfAny();
      printStart(module.start, f);
    }

    module.elems.forEach((elem) => {
      breakIfAny();
      printElem(elem, f);
    });

    module.datas.forEach((data) => {
      breakIfAny();
      printData(data, f);
    });

    f.breakDedent();
  });

  f.linebreak();
}

function todo(): never {
  throw new Error("todo");
}
