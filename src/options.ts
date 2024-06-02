import { ErrorFormat } from "./error";
import * as path from "node:path";
import * as fs from "node:fs";

export type Options = {
  input: string;
  filename: string;
  packageName: string;
  debug: Set<string>;
  noOutput: boolean;
  noStd: boolean;
  treatErrAsBug: boolean;
  errorFormat: ErrorFormat;
};

export function defaultOptions(): Options {
  return {
    filename: "",
    input: "",
    packageName: "main",
    debug: new Set<string>(),
    noOutput: false,
    noStd: false,
    treatErrAsBug: false,
    errorFormat: "text-render",
  };
}

export function parseArgs(hardcodedInput: string): Options {
  const opts = defaultOptions();

  if (process.argv.length > 2) {
    opts.filename = process.argv[2];
    if (path.extname(opts.filename) !== ".nil") {
      console.error(process.argv);

      console.error(
        `error: filename must have \`.nil\` extension: \`${opts.filename}\``,
      );
      process.exit(1);
    }

    opts.input = fs.readFileSync(opts.filename, { encoding: "utf-8" });
    if (opts.filename.endsWith(".mod.nil")) {
      opts.packageName = path.basename(opts.filename, ".mod.nil");
    } else {
      opts.packageName = path.basename(opts.filename, ".nil");
    }

    const debugArg = process.argv.find((arg) => arg.startsWith("--debug="));
    if (debugArg !== undefined) {
      const debugs = debugArg.slice("--debug=".length);
      opts.debug = new Set(debugs.split(","));
    }

    if (process.argv.some((arg) => arg === "--no-output")) {
      opts.noOutput = true;
    }
    if (process.argv.some((arg) => arg === "--no-std")) {
      opts.noStd = true;
    }
    if (process.argv.some((arg) => arg === "--treat-err-as-bug")) {
      opts.treatErrAsBug = true;
    }
    if (process.argv.some((arg) => arg === "--error-format-json")) {
      opts.errorFormat = "json";
    }
  } else {
    opts.filename = "<hardcoded>";
    opts.input = hardcodedInput;
    opts.packageName = "test";
    opts.debug = new Set([
      "tokens",
      "parsed",
      "resolved",
      "typecked",
      "wasm-validate",
    ]);
  }

  return opts;
}
