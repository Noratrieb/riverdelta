import { withErrorHandler } from "./error";
import { isValidIdent, tokenize } from "./lexer";
import { lower as lowerToWasm } from "./lower";
import { parse } from "./parser";
import { printAst } from "./printer";
import { resolve } from "./resolve";
import { typeck } from "./typeck";
import { writeModuleWatToString } from "./wasm/wat";
import fs from "fs";
import path from "path";
import { exec } from "child_process";
import { Ast, Built, Typecked } from "./ast";

const INPUT = `
function main() = (
  prIntln(0);
);

function prIntln(x: Int) = (
  print("\n");
);
`;

function main() {
  let input: string;
  let packageName: string;
  if (process.argv.length > 2) {
    const filename = process.argv[2];
    if (path.extname(filename) !== ".nil") {
      console.error(
        `error: filename must have \`.nil\` extension: \`${filename}\``
      );
      process.exit(1);
    }

    input = fs.readFileSync(filename, { encoding: "utf-8" });
    packageName = path.basename(filename, ".nil");
  } else {
    input = INPUT;
    packageName = "test";
  }

  if (!isValidIdent(packageName)) {
    console.error(
      `error: package name \`${packageName}\` is not a valid identifer`
    );
    process.exit(1);
  }

  console.log(`package name: '${packageName}'`);

  withErrorHandler(input, () => {
    const start = Date.now();

    const tokens = tokenize(input);
    console.log("-----TOKENS------------");
    console.log(tokens);

    const ast: Ast<Built> = parse(packageName, tokens);
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
    const typecked: Ast<Typecked> = typeck(resolved);
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
        console.error(`failed to spawn wasm-tools: ${error.message}`);
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
