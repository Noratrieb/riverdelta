import { Module } from "./defs";
import { writeModuleWatToString } from "./wat";

const EXAMPLE_MODULE: Module = {
  _name: "example",
  types: [
    { params: ["i32"], returns: ["i32"] },
    { params: [], returns: ["i32"] },
  ],
  imports: [
    {
      module: "left-pad",
      name: "padLeft",
      desc: {
        kind: "func",
        type: 0,
      },
    },
  ],
  funcs: [
    {
      _name: "addOne",
      type: 0,
      locals: ["i32", "i32"],
      body: [
        { kind: "local.set", imm: 0 },
        {
          kind: "block",
          type: { kind: "typeidx", idx: 1 },
          instrs: [{ kind: "local.get", imm: 0 }],
        },
        { kind: "i32.const", imm: 1 },
        { kind: "i32.add" },
      ],
    },
  ],
  tables: [
    {
      type: { reftype: "funcref", limits: { min: 10, max: 20 } },
      _name: "cool-table",
    },
  ],
  mems: [
    {
      type: { min: 100, max: 1000 },
      _name: "the_memory",
    },
  ],
  globals: [
    {
      type: { mut: "const", type: "i32" },
      init: [{ kind: "i32.const", imm: 0 }],
      _name: "globalling",
    },
  ],
  elems: [],
  exports: [
    {
      name: "addOne",
      desc: { kind: "func", idx: 0 },
    },
  ],
  datas: [
    {
      mode: { kind: "passive" },
      init: new Uint8Array(),
      _name: "meow",
    },
    {
      mode: {
        kind: "active",
        memory: 0,
        offset: [{ kind: "i32.const", imm: 0 }],
      },
      init: new Uint8Array(),
      _name: "very-active-data",
    },
  ],
};

it("should print a Wasm module with the correct formatting", () => {
  const wat = writeModuleWatToString(EXAMPLE_MODULE);

  expect(wat).toMatchInlineSnapshot(`
    "(module $example
      (type (func (param i32) (result i32)))
      (type (func (param) (result i32)))
      (import "left-pad" "padLeft" (func (type 0)))
      (func $addOne (type 0)
        (local i32 i32)
        local.set 0
        block (type 1)
          local.get 0
        end
        i32.const 1
        i32.add
      )
      (table $cool-table 10 20 funcref)
      (memory $the_memory 100 1000)
      (global $globalling i32
        i32.const 0
      )
      (export "addOne" (func 0))
      (data $meow "")
      (data $very-active-data (i32.const 0) "")
    )
    "
  `);
});
