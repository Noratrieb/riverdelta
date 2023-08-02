import {
  Crate,
  BUILTINS,
  Built,
  BuiltinName,
  Expr,
  Folder,
  Ident,
  Item,
  ItemId,
  LocalInfo,
  ModItem,
  Resolution,
  Resolved,
  mkDefaultFolder,
  superFoldExpr,
  superFoldItem,
  superFoldType,
  ExternItem,
} from "./ast";
import { GlobalContext } from "./context";
import { CompilerError, spanMerge } from "./error";
import { ComplexMap } from "./utils";

const BUILTIN_SET = new Set<string>(BUILTINS);

type Context = {
  ast: Crate<Built>;
  gcx: GlobalContext;
  modContentsCache: ComplexMap<ItemId, Map<string, ItemId>>;
  newItemsById: ComplexMap<ItemId, Item<Resolved>>;
};

function resolveModItem(
  cx: Context,
  mod: ModItem<Built> | ExternItem,
  item: Item<Built>,
  name: string
): ItemId | undefined {
  const cachedContents = cx.modContentsCache.get(item.id);
  if (cachedContents) {
    return cachedContents.get(name);
  }

  let contents: Map<string, ItemId>;

  if ("contents" in mod) {
    contents = new Map(mod.contents.map((item) => [item.node.name, item.id]));
  } else {
    const loadedCrate = cx.gcx.crateLoader(cx.gcx, item.node.name, item.span);
    contents = new Map(
      loadedCrate.rootItems.map((item) => [item.node.name, item.id])
    );
  }

  cx.modContentsCache.set(item.id, contents);
  return contents.get(name);
}

export function resolve(
  gcx: GlobalContext,
  ast: Crate<Built>
): Crate<Resolved> {
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
  };
}

function resolveModule(
  cx: Context,
  modName: string[],
  contents: Item<Built>[]
): Item<Resolved>[] {
  const items = new Map<string, ItemId>();

  contents.forEach((item) => {
    const existing = items.get(item.node.name);
    if (existing !== undefined) {
      throw new CompilerError(
        `item \`${item.node.name}\` has already been declared`,
        item.span
      );
    }
    items.set(item.node.name, item.id);
  });

  const scopes: string[] = [];

  const popScope = (expected: string) => {
    const popped = scopes.pop();
    if (popped !== expected) {
      throw new Error(
        `Scopes corrupted, wanted to pop ${expected} but popped ${popped}`
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

    const item = items.get(ident.name);
    if (item !== undefined) {
      return {
        kind: "item",
        id: item,
      };
    }

    if (ident.name === cx.ast.packageName) {
      return {
        kind: "item",
        id: new ItemId(cx.ast.id, 0),
      };
    }

    if (BUILTIN_SET.has(ident.name)) {
      return { kind: "builtin", name: ident.name as BuiltinName };
    }

    throw new CompilerError(`cannot find ${ident.name}`, ident.span);
  };

  const blockLocals: LocalInfo[][] = [];

  const resolver: Folder<Built, Resolved> = {
    ...mkDefaultFolder(),
    itemInner(item): Item<Resolved> {
      const defPath = [...modName, item.node.name];

      switch (item.kind) {
        case "function": {
          const params = item.node.params.map(({ name, span, type }) => ({
            name,
            span,
            type: this.type(type),
          }));
          const returnType =
            item.node.returnType && this.type(item.node.returnType);

          item.node.params.forEach(({ name }) => scopes.push(name));
          const body = this.expr(item.node.body);
          const revParams = item.node.params.slice();
          revParams.reverse();
          revParams.forEach(({ name }) => popScope(name));

          return {
            kind: "function",
            span: item.span,
            node: {
              name: item.node.name,
              params,
              returnType,
              body,
            },
            id: item.id,
            defPath,
          };
        }
        case "mod": {
          const contents = resolveModule(cx, defPath, item.node.contents);
          return {
            ...item,
            kind: "mod",
            node: { ...item.node, contents },
            defPath,
          };
        }
        case "extern": {
          const node: ExternItem = {
            ...item.node,
          };
          return {
            ...item,
            node,
            defPath,
          };
        }
      }

      return { ...superFoldItem(item, this), defPath };
    },
    expr(expr) {
      switch (expr.kind) {
        case "block": {
          const prevScopeLength = scopes.length;
          blockLocals.push([]);

          const exprs = expr.exprs.map<Expr<Resolved>>((inner) =>
            this.expr(inner)
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
              lhs.kind === "ident" ? resolveIdent(lhs.value) : lhs.res;
            const segments =
              lhs.kind === "ident" ? [lhs.value.name] : lhs.segments;

            if (res.kind === "item") {
              const module = cx.gcx.findItem(res.id, cx.ast);

              if (module.kind === "mod" || module.kind === "extern") {
                if (typeof expr.field.value === "number") {
                  throw new CompilerError(
                    "module contents cannot be indexed with a number",
                    expr.field.span
                  );
                }

                const pathResItem = resolveModItem(
                  cx,
                  module.node,
                  module,
                  expr.field.value
                );
                if (pathResItem === undefined) {
                  throw new CompilerError(
                    `module ${module.node.name} has no item ${expr.field.value}`,
                    expr.field.span
                  );
                }

                const pathRes: Resolution = { kind: "item", id: pathResItem };

                return {
                  kind: "path",
                  segments: [...segments, expr.field.value],
                  res: pathRes,
                  span: spanMerge(lhs.span, expr.field.span),
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
