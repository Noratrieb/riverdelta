import { withErrorHandler } from "./error";
import { tokenize } from "./lexer";
import { lower as lowerToWasm } from "./lower";
import { parse } from "./parser";
import { printAst } from "./printer";
import { resolve } from "./resolve";
import { typeck } from "./typeck";
import { writeModuleWatToString } from "./wasm/wat";
import fs from "fs";
import { exec } from "child_process";

const INPUT = `
function main() = (
  owo.uwu.meow();
);

mod owo (
  mod uwu (
    function meow() =;
  );
);
`;

function main() {
  let input: string;
  if (process.argv.length > 2) {
    input = fs.readFileSync(process.argv[2], { encoding: "utf-8" });
  } else {
    input = INPUT;
  }

  withErrorHandler(input, () => {
    const start = Date.now();

    const tokens = tokenize(input);
    console.log("-----TOKENS------------");
    console.log(tokens);

    const ast = parse(tokens);
    console.log("-----AST---------------");

    console.dir(ast.rootItems, { depth: 50 });

    console.log("-----AST pretty--------");
    const printed = printAst(ast);
    console.log(printed);

    console.log("-----AST resolved------");
    const resolved = resolve(ast);
    const resolvedPrinted = printAst(resolved);
    console.log(resolvedPrinted);

    console.log("-----AST typecked------");
    const typecked = typeck(resolved);
    const typeckPrinted = printAst(typecked);
    console.log(typeckPrinted);

    console.log("-----wasm--------------");
    const wasmModule = lowerToWasm(typecked);
    const moduleStringColor = writeModuleWatToString(wasmModule, true);
    const moduleString = writeModuleWatToString(wasmModule);

    console.log(moduleStringColor);

    fs.writeFileSync("out.wat", moduleString);

    console.log("--validate wasm-tools--");

    exec("wasm-tools validate out.wat", (error, stdout, stderr) => {
      if (error && error.code === 1) {
        console.log(stderr);
      } else if (error) {
        console.error(`failed to spawn wasm-tools: ${error}`);
      } else {
        if (stderr) {
          console.log(stderr);
        }
        if (stdout) {
          console.log(stdout);
        }
      }

      console.log(`finished in ${Date.now() - start}ms`);
    });
  });
}

main();
