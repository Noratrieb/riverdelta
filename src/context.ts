import { Crate, DepCrate, Final, Item, ItemId, Phase } from "./ast";
import { Span } from "./error";
import { Ids, unwrap } from "./utils";
import fs from "fs";
import path from "path";

export type CrateLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span,
) => DepCrate;

/**
 * The global context containing information about the _global compilation session_,
 * like loaded crates.
 * Notably, the global context is _not_ supposed to have information specific to the "local crate",
 * because with the current compilation model, there is no "local crate" in a session.
 *
 * There is a "downstream"/"binary"/"final" crate with crateId=0, where `function main()` lives, but
 * dependencies (which also use the same context) do not care about that.
 */
export class GlobalContext {
  public finalizedCrates: Crate<Final>[] = [];
  public crateId: Ids = new Ids();

  constructor(public opts: Options, public crateLoader: CrateLoader) {}

  public findItem<P extends Phase>(
    id: ItemId,
    localCrate?: Crate<P>,
  ): Item<P | Final> {
    const crate = unwrap(
      [...(localCrate ? [localCrate] : []), ...this.finalizedCrates].find(
        (crate) => crate.id === id.crateId,
      ),
    );

    if (id.itemIdx === 0) {
      // Return a synthetic module representing the crate root.
      return {
        kind: "mod",
        node: {
          contents: crate.rootItems,
          name: crate.packageName,
        },
        span: Span.startOfFile(crate.rootFile),
        id,
      };
    }

    return unwrap(crate.itemsById.get(id));
  }
}

export type Options = {
  input: string;
  filename: string;
  packageName: string;
  debug: Set<string>;
  noOutput: boolean;
};

export function parseArgs(hardcodedInput: string): Options {
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
        `error: filename must have \`.nil\` extension: \`${filename}\``,
      );
      process.exit(1);
    }

    input = fs.readFileSync(filename, { encoding: "utf-8" });
    if (filename.endsWith(".mod.nil")) {
      packageName = path.basename(filename, ".mod.nil");
    } else {
      packageName = path.basename(filename, ".nil");
    }

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
    input = hardcodedInput;
    packageName = "test";
    debug = new Set([
      "tokens",
      "parsed",
      "resolved",
      "typecked",
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
