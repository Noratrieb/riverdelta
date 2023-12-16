import {
  Pkg,
  BUILTINS,
  Built,
  BuiltinName,
  Expr,
  Folder,
  Ident,
  Item,
  ItemId,
  LocalInfo,
  ItemMod,
  Resolution,
  Resolved,
  mkDefaultFolder,
  superFoldExpr,
  superFoldItem,
  superFoldType,
  ItemExtern,
} from "./ast";
import { GlobalContext } from "./context";
import { CompilerError, ErrorEmitted, Span } from "./error";
import { ComplexMap } from "./utils";

const BUILTIN_SET = new Set<string>(BUILTINS);

type Context = {
  ast: Pkg<Built>;
  gcx: GlobalContext;
  modContentsCache: ComplexMap<ItemId, Map<string, ItemId>>;
  newItemsById: ComplexMap<ItemId, Item<Resolved>>;
};

function loadPkg(cx: Context, name: string, span: Span): Map<string, ItemId> {
  const loadedPkg = cx.gcx.pkgLoader(cx.gcx, name, span);

  const contents = new Map(
    loadedPkg.rootItems.map((item) => [item.name, item.id]),
  );

  return contents;
}

function resolveModItem(
  cx: Context,
  mod: ItemMod<Built> | ItemExtern<Built>,
  name: string,
): ItemId | undefined {
  const cachedContents = cx.modContentsCache.get(mod.id);
  if (cachedContents) {
    return cachedContents.get(name);
  }

  let contents: Map<string, ItemId>;

  if ("contents" in mod) {
    contents = new Map(mod.contents.map((item) => [item.name, item.id]));
  } else {
    contents = loadPkg(cx, mod.name, mod.span);
  }

  cx.modContentsCache.set(mod.id, contents);
  return contents.get(name);
}

export function resolve(gcx: GlobalContext, ast: Pkg<Built>): Pkg<Resolved> {
  const cx: Context = {
    ast,
    gcx,
    modContentsCache: new ComplexMap(),
    newItemsById: new ComplexMap(),
  };

  const rootItems = resolveModule(cx, [ast.packageName], ast.rootItems);
  return {
    id: ast.id,
    itemsById: cx.newItemsById,
    rootItems,
    packageName: ast.packageName,
    rootFile: ast.rootFile,
    fatalError: ast.fatalError,
  };
}

function resolveModule(
  cx: Context,
  modName: string[],
  contents: Item<Built>[],
): Item<Resolved>[] {
  const items = new Map<string, ItemId>();

  contents.forEach((item) => {
    const existing = items.get(item.name);
    if (existing !== undefined) {
      cx.gcx.error.emit(
        new CompilerError(
          `item \`${item.name}\` has already been declared`,
          item.span,
        ),
      );
    } else {
      items.set(item.name, item.id);
    }
  });

  const scopes: string[] = [];
  let tyParamScopes: string[] = [];

  const popScope = (expected: string) => {
    const popped = scopes.pop();
    if (popped !== expected) {
      throw new Error(
        `Scopes corrupted, wanted to pop ${expected} but popped ${popped}`,
      );
    }
  };

  const resolveIdent = (ident: Ident): Resolution => {
    const lastIdx = scopes.length - 1;
    for (let i = lastIdx; i >= 0; i--) {
      const candidate = scopes[i];
      if (candidate === ident.name) {
        const index = lastIdx - i;
        return {
          kind: "local",
          index,
        };
      }
    }

    for (let i = tyParamScopes.length - 1; i >= 0; i--) {
      const candidate = tyParamScopes[i];

      if (candidate === ident.name) {
        return {
          kind: "tyParam",
          index: i,
          name: ident.name,
        };
      }
    }

    const item = items.get(ident.name);
    if (item !== undefined) {
      return {
        kind: "item",
        id: item,
      };
    }

    // All loaded pkgs are in scope.
    for (const pkg of [cx.ast, ...cx.gcx.finalizedPkgs]) {
      if (ident.name === pkg.packageName) {
        return {
          kind: "item",
          id: ItemId.pkgRoot(pkg.id),
        };
      }
    }

    if (BUILTIN_SET.has(ident.name)) {
      return { kind: "builtin", name: ident.name as BuiltinName };
    }

    return {
      kind: "error",
      err: cx.gcx.error.emit(
        new CompilerError(`cannot find ${ident.name}`, ident.span),
      ),
    };
  };

  const blockLocals: LocalInfo[][] = [];

  const resolver: Folder<Built, Resolved> = {
    ...mkDefaultFolder(),
    itemInner(item): Item<Resolved> {
      const defPath = [...modName, item.name];

      switch (item.kind) {
        case "function": {
          const params = item.params.map(({ name, span, type }) => ({
            name,
            span,
            type: this.type(type),
          }));
          const returnType = item.returnType && this.type(item.returnType);

          item.params.forEach(({ name }) => scopes.push(name));
          const body = this.expr(item.body);
          const revParams = item.params.slice();
          revParams.reverse();
          revParams.forEach(({ name }) => popScope(name));

          return {
            kind: "function",
            span: item.span,
            name: item.name,
            params,
            returnType,
            body,

            id: item.id,
            defPath,
          };
        }
        case "mod": {
          const contents = resolveModule(cx, defPath, item.contents);
          return {
            ...item,
            kind: "mod",
            contents,
            defPath,
          };
        }
        case "extern": {
          // Eagerly resolve the pkg.
          // Note that because you can reference extern pkgs before the item,
          // we still need the loadPkg in the field access code above.
          loadPkg(cx, item.name, item.span);

          return {
            ...item,
            defPath,
          };
        }
        case "type": {
          tyParamScopes = item.generics.map(({ name }) => name);

          const type = { ...superFoldItem(item, this) };

          return type;
        }
      }

      return { ...superFoldItem(item, this), defPath };
    },
    expr(expr): Expr<Resolved> {
      switch (expr.kind) {
        case "block": {
          const prevScopeLength = scopes.length;
          blockLocals.push([]);

          const exprs = expr.exprs.map<Expr<Resolved>>((inner) =>
            this.expr(inner),
          );

          scopes.length = prevScopeLength;
          const locals = blockLocals.pop();

          return {
            kind: "block",
            exprs,
            locals,
            span: expr.span,
          };
        }
        case "let": {
          const rhs = this.expr(expr.rhs);
          const type = expr.type && this.type(expr.type);

          scopes.push(expr.name.name);
          const local = { name: expr.name.name, span: expr.name.span };
          blockLocals[blockLocals.length - 1].push(local);

          return {
            ...expr,
            name: expr.name,
            local,
            type,
            rhs,
          };
        }
        case "fieldAccess": {
          // We convert field accesses to paths if the lhs refers to a module.

          const lhs = this.expr(expr.lhs);

          if (lhs.kind === "ident" || lhs.kind === "path") {
            const res =
              lhs.kind === "ident" ? resolveIdent(lhs.value) : lhs.value.res;
            const segments =
              lhs.kind === "ident" ? [lhs.value.name] : lhs.segments;

            if (res.kind === "item") {
              const module = cx.gcx.findItem(res.id, cx.ast);

              if (module.kind === "mod" || module.kind === "extern") {
                let pathRes: Resolution;

                if (typeof expr.field.value === "number") {
                  const err: ErrorEmitted = cx.gcx.error.emit(
                    new CompilerError(
                      "module contents cannot be indexed with a number",
                      expr.field.span,
                    ),
                  );
                  pathRes = { kind: "error", err };
                } else {
                  const pathResItem = resolveModItem(
                    cx,
                    module,
                    expr.field.value,
                  );

                  if (pathResItem === undefined) {
                    const err: ErrorEmitted = cx.gcx.error.emit(
                      new CompilerError(
                        `module ${module.name} has no item ${expr.field.value}`,
                        expr.field.span,
                      ),
                    );
                    pathRes = { kind: "error", err };
                  } else {
                    pathRes = { kind: "item", id: pathResItem };
                  }
                }
                const span = lhs.span.merge(expr.field.span);
                return {
                  kind: "path",
                  segments: [...segments, String(expr.field.value)],
                  value: { res: pathRes, span },
                  span,
                };
              }
            }
          }

          return superFoldExpr(expr, this);
        }
        default: {
          return superFoldExpr(expr, this);
        }
      }
    },
    ident(ident) {
      const res = resolveIdent(ident);
      return { name: ident.name, span: ident.span, res };
    },
    type(type) {
      return superFoldType(type, this);
    },
    newItemsById: cx.newItemsById,
  };

  return contents.map((item) => resolver.item(item));
}
