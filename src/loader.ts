import { PkgId, DepPkg } from "./ast";
import { PkgLoader, GlobalContext } from "./context";
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

function dummyErrorPkg(
  id: PkgId,
  packageName: string,
  emitted: ErrorEmitted,
): DepPkg {
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

export const loadPkg: PkgLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span,
): DepPkg => {
  // If we've loaded the pkg already, great.
  const existing = gcx.finalizedPkgs.find((pkg) => pkg.packageName === name);
  if (existing) {
    return existing;
  }

  const pkgId = gcx.pkgId.next();

  // If we have not loaded the pkg yet, we may actually already be loading it.
  // A cycle!!
  if (gcx.pkgsBeingLoaded.has(name)) {
    return dummyErrorPkg(
      pkgId,
      name,
      gcx.error.emit(
        new CompilerError(`cycle detected loading extern module ${name}`, span),
      ),
    );
  }

  // Let's start loading the pkg!
  gcx.pkgsBeingLoaded.add(name);

  // We really, really want a good algorithm for finding pkgs.
  // But right now we just look for files in the CWD.

  const file = loadModuleFile(".", name, span);
  if (!file.ok) {
    return dummyErrorPkg(pkgId, name, gcx.error.emit(file.err));
  }

  const tokens = tokenize(gcx.error, file.value);
  if (!tokens.ok) {
    return dummyErrorPkg(pkgId, name, tokens.err);
  }
  const parseState: ParseState = {
    tokens: tokens.tokens,
    file: file.value,
    gcx,
  };
  const ast = parse(name, parseState, pkgId);
  const resolved = resolve(gcx, ast);

  const typecked = typeck(gcx, resolved);

  gcx.finalizedPkgs.push(typecked);
  // Pkg is loaded, no cycles.
  gcx.pkgsBeingLoaded.delete(name);
  return typecked;
};
