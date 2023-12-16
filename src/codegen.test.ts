import { ItemId } from "./ast";
import { layoutOfStruct } from "./codegen";
import { TY_I32, TY_INT, TyStruct } from "./types";

it("should compute struct layout correctly", () => {
  const ty: TyStruct = {
    kind: "struct",
    itemId: ItemId.dummy(),
    genericArgs: [],
    params: [],
    _name: "",
    fields_no_subst: [
      ["uwu", TY_I32],
      ["owo", TY_INT],
    ],
  };

  const layout = layoutOfStruct(ty);

  expect(layout).toMatchInlineSnapshot(`
    {
      "align": 8,
      "fields": [
        {
          "ty": {
            "kind": "i32",
          },
          "types": [
            {
              "offset": 4,
              "type": "i32",
            },
          ],
        },
        {
          "ty": {
            "kind": "int",
          },
          "types": [
            {
              "offset": 8,
              "type": "i64",
            },
          ],
        },
      ],
      "size": 16,
    }
  `);
});

it("should compute single field struct layout correctly", () => {
  const ty: TyStruct = {
    kind: "struct",
    itemId: ItemId.dummy(),
    genericArgs: [],
    params: [],
    _name: "",
    fields_no_subst: [["owo", TY_INT]],
  };

  const layout = layoutOfStruct(ty);

  expect(layout).toMatchInlineSnapshot(`
    {
      "align": 8,
      "fields": [
        {
          "ty": {
            "kind": "int",
          },
          "types": [
            {
              "offset": 4,
              "type": "i64",
            },
          ],
        },
      ],
      "size": 8,
    }
  `);
});
