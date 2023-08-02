import { tokenize } from "./lexer";

it("should tokenize an emtpy function", () => {
  const input = `function hello() = ;`;

  const tokens = tokenize({ content: input });

  expect(tokens).toMatchSnapshot();
});

it("should tokenize hello world", () => {
  const input = `print("hello world")`;

  const tokens = tokenize({ content: input });

  expect(tokens).toMatchSnapshot();
});
