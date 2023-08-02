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
import { Crate, Built, Typecked, DepCrate } from "./ast";
import { GlobalContext, CrateLoader } from "./context";

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

type Config = {
  input: string;
  filename: string;
  packageName: string;
  debug: Set<string>;
  noOutput: boolean;
};

function parseArgs(): Config {
  let filename: string;
  let input: string;
  let packageName: string;
  let debug = new Set<string>();
  let noOutput = false;

  if (process.argv.length > 2) {
    filename = process.argv[2];
    if (path.extname(filename) !== ".nil") {
      console.error(process.argv);

      console.error(
        `error: filename must have \`.nil\` extension: \`${filename}\``
      );
      process.exit(1);
    }

    input = fs.readFileSync(filename, { encoding: "utf-8" });
    packageName = path.basename(filename, ".nil");

    const debugArg = process.argv.find((arg) => arg.startsWith("--debug="));
    if (debugArg !== undefined) {
      const debugs = debugArg.slice("--debug=".length);
      debug = new Set(debugs.split(","));
    }

    if (process.argv.some((arg) => arg === "--no-output")) {
      noOutput = true;
    }
  } else {
    filename = "<hardcoded>";
    input = INPUT;
    packageName = "test";
    debug = new Set([
      "tokens",
      "parsed",
      "resolved",
      "typecked",
      "wat",
      "wasm-validate",
    ]);
  }

  return {
    filename,
    input,
    packageName,
    debug,
    noOutput,
  };
}

function main() {
  const config = parseArgs();
  const { filename, packageName, input, debug } = config;

  if (!isValidIdent(packageName)) {
    console.error(
      `error: package name \`${packageName}\` is not a valid identifer`
    );
    process.exit(1);
  }

  const gcx = new GlobalContext(loadCrate);
  const mainCrate = gcx.crateId.next();

  withErrorPrinter(
    input,
    filename,
    () => {
      const start = Date.now();

      const tokens = tokenize(input);
      if (debug.has("tokens")) {
        console.log("-----TOKENS------------");
        console.log(tokens);
      }

      const ast: Crate<Built> = parse(packageName, tokens, mainCrate);
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
      const wasmModule = lowerToWasm([typecked, ...gcx.depCrates]);
      const moduleStringColor = writeModuleWatToString(wasmModule, true);
      const moduleString = writeModuleWatToString(wasmModule);

      if (debug.has("wat")) {
        console.log(moduleStringColor);
      }

      if (!config.noOutput) {
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
    () => process.exit(1)
  );
}

const loadCrate: CrateLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span
): DepCrate => {
  // We really, really want a good algorithm for finding crates.
  // But right now we just look for files in the CWD.

  const existing = gcx.depCrates.find((crate) => crate.packageName === name);
  if (existing) {
    return existing;
  }

  const options = [`${name}.nil`, `${name}/${name}.mod.nil`];

  let input: string | undefined = undefined;
  let filename: string | undefined = undefined;
  options.forEach((tryName) => {
    try {
      input = fs.readFileSync(tryName, { encoding: "utf-8" });
      filename = tryName;
    } catch (e) {}
  });

  if (input === undefined || filename === undefined) {
    throw new CompilerError(
      `failed to load ${name}, could not find ${options.join(" or ")}`,
      span
    );
  }

  const inputString: string = input;

  return withErrorPrinter(
    inputString,
    filename,
    (): DepCrate => {
      const crateId = gcx.crateId.next();

      const tokens = tokenize(inputString);
      const ast = parse(name, tokens, crateId);
      const resolved = resolve(gcx, ast);
      console.log(resolved);

      const typecked = typeck(gcx, resolved);

      gcx.depCrates.push(typecked);
      return typecked;
    },
    () => {
      throw new CompilerError(
        `failed to load crate ${name}: crate contains errors`,
        span
      );
    }
  );
};

main();
