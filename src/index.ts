import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { parse } from "./parser";

const input = `
function hello() {}
`;

function main() {
  withErrorHandler(input, () => {
    const tokens = tokenize(input);
    console.log(tokens);

    const ast = parse(tokens);
    console.log(ast);
  });
}

main();
