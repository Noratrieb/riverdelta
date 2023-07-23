import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { parse } from "./parser";
import { printAst } from "./printer";
import { resolve } from "./resolve";
import { typeck } from "./typeck";

const input = `
function main() = (
  let true = false in print(true)
);
`;

function main() {
  withErrorHandler(input, () => {
    const tokens = tokenize(input);
    console.log("-----TOKENS---");
    console.log(tokens);

    const ast = parse(tokens);
    console.log("-----AST------");

    console.dir(ast, { depth: 50 });

    const printed = printAst(ast);
    console.log("-----AST pretty------");
    console.log(printed);

    const resolved = resolve(ast);
    console.log("-----AST resolved------");
    const resolvedPrinted = printAst(resolved);
    console.log(resolvedPrinted);

    console.log("-----AST typecked------");

    const typecked = typeck(resolved);
    console.dir(typecked, { depth: 10 });
  });
}

main();
