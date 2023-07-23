import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { parse } from "./parser";
import { printAst } from "./printer";
import { resolve } from "./resolve";

const input = `
function main(argv: [String]): () = (
  print(argv);
  if 1 then (
    print("AAAAAAAAAAAAAAAAAAAA");
    let a = 0 in
    a;
  ) else (
    print("AAAAAAAAAAAAAAAAAAAAAA");
    let b = 0 in
    b;
  )
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
  });
}

main();
