import { ErrorHandler, unreachable } from "./error";
import { tokenize } from "./lexer";

it("should tokenize an emtpy function", () => {
  const input = `function hello() = ;`;

  const tokens = tokenize(new ErrorHandler(false), { content: input });
  if (!tokens.ok) unreachable("lexer error");

  expect(tokens.tokens).toMatchSnapshot();
});

it("should tokenize hello world", () => {
  const input = `print("hello world")`;

  const tokens = tokenize(new ErrorHandler(false), { content: input });
  if (!tokens.ok) unreachable("lexer error");

  expect(tokens.tokens).toMatchSnapshot();
});
