import { DepCrate } from "./ast";
import { CrateLoader, GlobalContext } from "./context";
import { CompilerError, LoadedFile, Span, withErrorPrinter } from "./error";
import fs from "fs";
import path from "path";
import { tokenize } from "./lexer";
import { ParseState, parse } from "./parser";
import { resolve } from "./resolve";
import { typeck } from "./typeck";

export function loadModuleFile(
  relativeTo: string,
  moduleName: string,
  span: Span
): LoadedFile {
  const options = [
    path.join(relativeTo, `${moduleName}.nil`),
    path.join(relativeTo, moduleName, `${moduleName}.mod.nil`),
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
    throw new CompilerError(
      `failed to load ${moduleName}, could not find ${options.join(" or ")}`,
      span
    );
  }

  return { content, path: filePath };
}

export const loadCrate: CrateLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span
): DepCrate => {
  // We really, really want a good algorithm for finding crates.
  // But right now we just look for files in the CWD.

  const existing = gcx.finalizedCrates.find((crate) => crate.packageName === name);
  if (existing) {
    return existing;
  }

  const file = loadModuleFile(".", name, span);

  return withErrorPrinter(
    (): DepCrate => {
      const crateId = gcx.crateId.next();

      const tokens = tokenize(file);
      const parseState: ParseState = { tokens, file };
      const ast = parse(name, parseState, crateId);
      const resolved = resolve(gcx, ast);
      console.log(resolved);

      const typecked = typeck(gcx, resolved);

      gcx.finalizedCrates.push(typecked);
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
