import { CrateId, DepCrate } from "./ast";
import { CrateLoader, GlobalContext } from "./context";
import { CompilerError, ErrorEmitted, LoadedFile, Span } from "./error";
import fs from "fs";
import path from "path";
import { tokenize } from "./lexer";
import { ParseState, parse } from "./parser";
import { resolve } from "./resolve";
import { typeck } from "./typeck";
import { ComplexMap } from "./utils";

export type LoadResult<T> =
  | {
      ok: true;
      value: T;
    }
  | {
      ok: false;
      err: CompilerError;
    };

export function loadModuleFile(
  relativeTo: string,
  moduleName: string,
  span: Span,
): LoadResult<LoadedFile> {
  let searchDir: string;
  if (relativeTo.endsWith(".mod.nil")) {
    // x/uwu.mod.nil searches in x/
    searchDir = path.dirname(relativeTo);
  } else if (relativeTo.endsWith(".nil")) {
    return {
      ok: false,
      err: new CompilerError(
        `.nil files cannot have submodules. use .mod.nil in a subdirectory`,
        span,
      ),
    };
  } else {
    searchDir = relativeTo;
  }

  const options = [
    path.join(searchDir, `${moduleName}.nil`),
    path.join(searchDir, moduleName, `${moduleName}.mod.nil`),
  ];

  let content: string | undefined = undefined;
  let filePath: string | undefined = undefined;
  options.forEach((tryPath) => {
    try {
      content = fs.readFileSync(tryPath, { encoding: "utf-8" });
      filePath = tryPath;
    } catch (e) {}
  });

  if (content === undefined || filePath === undefined) {
    return {
      ok: false,
      err: new CompilerError(
        `failed to load ${moduleName}, could not find ${options.join(" or ")}`,
        span,
      ),
    };
  }

  return { ok: true, value: { content, path: filePath } };
}

function dummyErrorCrate(
  id: CrateId,
  packageName: string,
  emitted: ErrorEmitted,
): DepCrate {
  return {
    id,
    packageName,
    rootItems: [],
    itemsById: new ComplexMap(),
    rootFile: { content: "<dummy>" },
    fatalError: emitted,
    typeckResults: {
      main: undefined,
    },
  };
}

export const loadCrate: CrateLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span,
): DepCrate => {
  // We really, really want a good algorithm for finding crates.
  // But right now we just look for files in the CWD.

  const existing = gcx.finalizedCrates.find(
    (crate) => crate.packageName === name,
  );
  if (existing) {
    return existing;
  }

  const crateId = gcx.crateId.next();

  const file = loadModuleFile(".", name, span);
  if (!file.ok) {
    return dummyErrorCrate(crateId, name, gcx.error.emit(file.err));
  }

  const tokens = tokenize(gcx.error, file.value);
  if (!tokens.ok) {
    return dummyErrorCrate(crateId, name, tokens.err);
  }
  const parseState: ParseState = {
    tokens: tokens.tokens,
    file: file.value,
    gcx,
  };
  const ast = parse(name, parseState, crateId);
  const resolved = resolve(gcx, ast);

  const typecked = typeck(gcx, resolved);

  gcx.finalizedCrates.push(typecked);
  return typecked;
};
