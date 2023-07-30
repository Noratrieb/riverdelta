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
  prIntln(0);
  prIntln(1);
  prIntln(9);
  prIntln(2352353);
  prIntln(100);
);

function prIntln(x: Int) = (
  prInt(x);
  print("\n");
);

function stringForDigit(x: Int): String = 
  if x == 0 then "0"
  else if x == 1 then "1"
  else if x == 2 then "2"
  else if x == 3 then "3"
  else if x == 4 then "4"
  else if x == 5 then "5"
  else if x == 6 then "6"
  else if x == 7 then "7"
  else if x == 8 then "8"
  else if x == 9 then "9"
  else trap();

function log10(x: Int): Int = (
  let i = 0;
  loop (
    if x < 10 then break;
    i = i + 1;
    x = x / 10;
  );
  i
);

function pow(base: Int, exp: Int): Int = (
  let acc = 1;
  loop (
    if exp == 0 then break;
    acc = acc * base;
    exp = exp - 1;
  );
  acc
);

function prInt(x: Int) = (
  let mag = log10(x);

  loop (
    if mag == 0 then break;
    let base = pow(10, mag);

    let digit = x / base;
    print(stringForDigit(digit));

    x = x % base;
    mag = mag - 1;
  );

  print(stringForDigit(x % 10));
);

function println(s: String) = (
  print(s);
  print("\n");
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
