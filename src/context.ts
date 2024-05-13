import { Pkg, DepPkg, Final, Item, ItemId, Phase } from "./ast";
import { ErrorHandler, Span } from "./error";
import { Ids, unwrap } from "./utils";
import fs from "fs";
import path from "path";

export type PkgLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span,
) => DepPkg;

/**
 * The global context containing information about the _global compilation session_,
 * like loaded pkgs.
 * Notably, the global context is _not_ supposed to have information specific to the "local pkg",
 * because with the current compilation model, there is no "local pkg" in a session.
 *
 * There is a "downstream"/"binary"/"final" pkg with pkgId=0, where `function main()` lives, but
 * dependencies (which also use the same context) do not care about that.
 */
export class GlobalContext {
  public error: ErrorHandler = new ErrorHandler();
  public finalizedPkgs: Pkg<Final>[] = [];
  // For cycle detection.
  public pkgsBeingLoaded: Set<string> = new Set<string>();
  public pkgId: Ids = new Ids();

  constructor(public opts: Options, public pkgLoader: PkgLoader) {}

  public findItem<P extends Phase>(
    id: ItemId,
    localPkg?: Pkg<P>,
  ): Item<P> | Item<Final> {
    const allPkgs: (Pkg<P> | Pkg<Final>)[] = [
      ...(localPkg ? [localPkg] : []),
      ...this.finalizedPkgs,
    ];

    const pkg: Pkg<P> | Pkg<Final> = unwrap(
      allPkgs.find((pkg) => pkg.id === id.pkgId),
    );

    if (pkg.fatalError) {
      return {
        kind: "error",
        defPath: [],
        err: pkg.fatalError,
        id: new ItemId(pkg.id, 0),
        name: "",
        span: Span.startOfFile(pkg.rootFile),
      };
    }

    if (id.itemIdx === 0) {
      const contents: Item<P>[] | Item<Final>[] = pkg.rootItems;
      // Typescript does not seem to be able to understand this here.
      // The type of this is supposed to be (Item<P> | Item<Final>)["contents"] which is
      // "too complex to represent".
      const erasedContents: any = contents;

      // Return a synthetic module representing the pkg root.
      const mod: Item<P> | Item<Final> = {
        kind: "mod",
        name: pkg.packageName,
        contents: erasedContents,
        span: Span.startOfFile(pkg.rootFile),
        id,
      };
      return mod;
    }

    const mod = unwrap(pkg.itemsById.get(id));
    return mod;
  }
}

export type Options = {
  input: string;
  filename: string;
  packageName: string;
  debug: Set<string>;
  noOutput: boolean;
  noStd: boolean;
};

export function parseArgs(hardcodedInput: string): Options {
  let filename: string;
  let input: string;
  let packageName: string;
  let debug = new Set<string>();
  let noOutput = false;
  let noStd = false;

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
    if (process.argv.some((arg) => arg === "--no-std")) {
      noStd = true;
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
    noStd,
  };
}
