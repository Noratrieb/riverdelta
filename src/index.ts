import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { parse } from "./parser";
import { printAst } from "./printer";

const input = `
function main(argv: [String]): uwu = (
  print("Hello, world!");
  let a: [String] = 0 in
  let b = 1 in
  if 0 then 0 else (
    if 1 == 1 then 1 else "what" 
  ;"meow")
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
