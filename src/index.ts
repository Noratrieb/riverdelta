import { LoadedFile, Span, withErrorPrinter } from "./error";
import { isValidIdent, tokenize } from "./lexer";
import { lower as lowerToWasm } from "./codegen";
import { ParseState, parse } from "./parser";
import { printAst } from "./printer";
import { resolve } from "./resolve";
import { typeck } from "./typeck";
import { writeModuleWatToString } from "./wasm/wat";
import fs from "fs";
import { exec } from "child_process";
import { Crate, Built, Typecked } from "./ast";
import { GlobalContext, parseArgs } from "./context";
import { loadCrate } from "./loader";

const INPUT = `
type A = { a: Int };

function main() = (
  uwu();
);

function uwu() = (
  let a = A { a: 100 };
  eat(a /*+1*/);

  A { a: 100 };

  /*-1*/
);

type B = {
  a: (Int, Int, Int, Int, Int),
};

function test(b: B) = (
  b.a;
); 

function eat(a: A) =;
`;

function main() {
  const opts = parseArgs(INPUT);
  const { filename, packageName, input, debug } = opts;

  if (!isValidIdent(packageName)) {
    console.error(
      `error: package name \`${packageName}\` is not a valid identifer`,
    );
    process.exit(1);
  }

  const file: LoadedFile = { path: filename, content: input };

  const gcx = new GlobalContext(opts, loadCrate);
  const mainCrate = gcx.crateId.next();

  withErrorPrinter(
    () => {
      const start = Date.now();

      gcx.crateLoader(gcx, "std", Span.startOfFile(file));

      const tokens = tokenize(file);
      if (debug.has("tokens")) {
        console.log("-----TOKENS------------");
        console.log(tokens);
      }

      const parseState: ParseState = { tokens, file };

      const ast: Crate<Built> = parse(packageName, parseState, mainCrate);
      if (debug.has("ast")) {
        console.log("-----AST---------------");

        console.dir(ast.rootItems, { depth: 50 });

        console.log("-----AST pretty--------");
        const printed = printAst(ast);
        console.log(printed);
      }

      if (debug.has("resolved")) {
        console.log("-----AST resolved------");
      }
      const resolved = resolve(gcx, ast);
      if (debug.has("resolved")) {
        const resolvedPrinted = printAst(resolved);
        console.log(resolvedPrinted);
      }

      if (debug.has("typecked")) {
        console.log("-----AST typecked------");
      }
      const typecked: Crate<Typecked> = typeck(gcx, resolved);
      if (debug.has("typecked")) {
        const typeckPrinted = printAst(typecked);
        console.log(typeckPrinted);
      }

      if (debug.has("wat")) {
        console.log("-----wasm--------------");
      }

      gcx.finalizedCrates.push(typecked);
      const wasmModule = lowerToWasm(gcx);
      const moduleStringColor = writeModuleWatToString(wasmModule, true);
      const moduleString = writeModuleWatToString(wasmModule);

      if (debug.has("wat")) {
        console.log(moduleStringColor);
      }

      if (!opts.noOutput) {
        fs.writeFileSync("out.wat", moduleString);
      }

      if (debug.has("wasm-validate")) {
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
      }
    },
    () => process.exit(1),
  );
}

main();
