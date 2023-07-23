import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { parse } from "./parser";
import { printAst } from "./printer";

const input = `
function main() = (
  print("Hello, world!");
  "uwu";
);
`;

function main() {
  withErrorHandler(input, () => {
    const tokens = tokenize(input);
    console.log("-----TOKENS---");
    console.log(tokens);

    const ast = parse(tokens);
    console.log("-----AST------");
    
    console.dir(ast, { depth: 10 });

    const printed = printAst(ast);
    console.log("-----AST pretty------");
    console.log(printed);
    
  });
}

main();
