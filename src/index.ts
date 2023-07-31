import { CompilerError, Span, withErrorPrinter } from "./error";
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
import { Crate, Built, Typecked } from "./ast";
import { Ids } from "./utils";

const INPUT = `
extern mod std;

type A = { a: Int };

function main() = (
  let a = A { a: 100 };
  printA(a);
);

function printA(a: A) = (
  print("ABCDEFGH\\n");
  std.printlnInt(a.a);
  print("ABCDEFGH\\n");
);

function linkStd() = (
  std.println("a");
);
`;

function main() {
  let filename: string;
  let input: string;
  let packageName: string;
  if (process.argv.length > 2) {
    filename = process.argv[2];
    if (path.extname(filename) !== ".nil") {
      console.error(
        `error: filename must have \`.nil\` extension: \`${filename}\``
      );
      process.exit(1);
    }

    input = fs.readFileSync(filename, { encoding: "utf-8" });
    packageName = path.basename(filename, ".nil");
  } else {
    filename = "<hardcoded>";
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

  withErrorPrinter(input, filename, () => {
    const start = Date.now();

    const tokens = tokenize(input);
    console.log("-----TOKENS------------");
    console.log(tokens);

    const ast: Crate<Built> = parse(packageName, tokens, 0);
    console.log("-----AST---------------");

    console.dir(ast.rootItems, { depth: 50 });

    console.log("-----AST pretty--------");
    const printed = printAst(ast);
    console.log(printed);

    console.log("-----AST resolved------");
    const [resolved, crates] = resolve(ast, loadCrate);
    const resolvedPrinted = printAst(resolved);
    console.log(resolvedPrinted);

    console.log("-----AST typecked------");
    const typecked: Crate<Typecked> = typeck(resolved, crates);
    const typeckPrinted = printAst(typecked);
    console.log(typeckPrinted);

    console.log("-----wasm--------------");
    const wasmModule = lowerToWasm([typecked, ...crates]);
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

function loadCrate(
  name: string,
  span: Span,
  crateId: Ids,
  existingCrates: Crate<Typecked>[]
): [Crate<Typecked>, Crate<Typecked>[]] {
  // We really, really want a good algorithm for finding crates.
  // But right now we just look for files in the CWD.

  const existing = existingCrates.find((crate) => crate.packageName === name);
  if (existing) {
    return [existing, []];
  }

  const filename = `${name}.nil`;
  let input;
  try {
    input = fs.readFileSync(filename, { encoding: "utf-8" });
  } catch (e) {
    throw new CompilerError(
      `failed to load ${name}, could not fine \`${filename}\``,
      span
    );
  }

  try {
    const tokens = tokenize(input);
    const ast = parse(name, tokens, crateId.next());
    const [resolved, crates] = resolve(ast, loadCrate);
    console.log(resolved);
    

    const typecked = typeck(resolved, [...existingCrates, ...crates]);
    return [typecked, crates];
  } catch (e) {
    withErrorPrinter(input, filename, () => {
      throw e;
    });
    throw new CompilerError(
      `failed to load crate ${name}: crate contains errors`,
      span
    );
  }
}

main();
