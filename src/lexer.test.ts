import { ErrorHandler, unreachable } from "./error";
import { tokenize } from "./lexer";

it("should tokenize an emtpy function", () => {
  const input = `function hello() = ;`;

  const tokens = tokenize(new ErrorHandler(false), { content: input });
  if (!tokens.ok) unreachable("lexer error");

  expect(tokens.tokens).toMatchInlineSnapshot(`
    [
      {
        "kind": "function",
        "span": Span {
          "end": 8,
          "file": {
            "content": "function hello() = ;",
          },
          "start": 0,
        },
      },
      {
        "ident": "hello",
        "kind": "identifier",
        "span": Span {
          "end": 14,
          "file": {
            "content": "function hello() = ;",
          },
          "start": 9,
        },
      },
      {
        "kind": "(",
        "span": Span {
          "end": 15,
          "file": {
            "content": "function hello() = ;",
          },
          "start": 14,
        },
      },
      {
        "kind": ")",
        "span": Span {
          "end": 16,
          "file": {
            "content": "function hello() = ;",
          },
          "start": 15,
        },
      },
      {
        "kind": "=",
        "span": Span {
          "end": 18,
          "file": {
            "content": "function hello() = ;",
          },
          "start": 17,
        },
      },
      {
        "kind": ";",
        "span": Span {
          "end": 20,
          "file": {
            "content": "function hello() = ;",
          },
          "start": 19,
        },
      },
    ]
  `);
});

it("should tokenize hello world", () => {
  const input = `print("hello world")`;

  const tokens = tokenize(new ErrorHandler(false), { content: input });
  if (!tokens.ok) unreachable("lexer error");

  expect(tokens.tokens).toMatchInlineSnapshot(`
    [
      {
        "ident": "print",
        "kind": "identifier",
        "span": Span {
          "end": 5,
          "file": {
            "content": "print("hello world")",
          },
          "start": 0,
        },
      },
      {
        "kind": "(",
        "span": Span {
          "end": 6,
          "file": {
            "content": "print("hello world")",
          },
          "start": 5,
        },
      },
      {
        "kind": "lit_string",
        "span": Span {
          "end": 19,
          "file": {
            "content": "print("hello world")",
          },
          "start": 6,
        },
        "value": "hello world",
      },
      {
        "kind": ")",
        "span": Span {
          "end": 20,
          "file": {
            "content": "print("hello world")",
          },
          "start": 19,
        },
      },
    ]
  `);
});
