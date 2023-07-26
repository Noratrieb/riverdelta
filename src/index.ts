import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { lower as lowerToWasm } from "./lower";
import { parse } from "./parser";
import { printAst } from "./printer";
import { resolve } from "./resolve";
import { typeck } from "./typeck";
import { writeModuleWatToString } from "./wasm/wat";

const input = `
function main(i: Int): Int = 0;
`;

function main() {
  withErrorHandler(input, () => {
    const tokens = tokenize(input);
    console.log("-----TOKENS------------");
    console.log(tokens);

    const ast = parse(tokens);
    console.log("-----AST---------------");

    console.dir(ast, { depth: 50 });

    const printed = printAst(ast);
    console.log("-----AST pretty--------");
    console.log(printed);

    const resolved = resolve(ast);
    console.log("-----AST resolved------");
    const resolvedPrinted = printAst(resolved);
    console.log(resolvedPrinted);

    console.log("-----AST typecked------");

    const typecked = typeck(resolved);
    
    console.log("-----wasm--------------");
    const wasmModule = lowerToWasm(typecked);
    const moduleString = writeModuleWatToString(wasmModule);
    console.log(moduleString);
  });
}

main();
