import {
  Ast,
  BUILTINS,
  BuiltinName,
  DEFAULT_FOLDER,
  Expr,
  Folder,
  Identifier,
  Item,
  ItemId,
  LocalInfo,
  ModItem,
  Resolution,
  superFoldExpr,
  superFoldItem,
} from "./ast";
import { CompilerError, spanMerge, todo } from "./error";
import { unwrap } from "./utils";

const BUILTIN_SET = new Set<string>(BUILTINS);

type Context = {
  ast: Ast;
  modContentsCache: Map<ItemId, Map<string, ItemId>>;
};

function resolveModItem(
  cx: Context,
  mod: ModItem,
  modId: ItemId,
  name: string
): ItemId | undefined {
  const cachedContents = cx.modContentsCache.get(modId);
  if (cachedContents) {
    return cachedContents.get(name);
  }

  switch (mod.modKind.kind) {
    case "inline": {
      const contents = new Map(
        mod.modKind.contents.map((item) => [item.node.name, item.id])
      );      
      cx.modContentsCache.set(modId, contents);
      return contents.get(name);
    }
    case "extern": {
      todo("extern mod items");
    }
  }
}

export function resolve(ast: Ast): Ast {
  const cx: Context = { ast, modContentsCache: new Map() };

  const rootItems = resolveModule(cx, ast.rootItems);
  return { ...ast, rootItems };
}

function resolveModule(cx: Context, contents: Item[]): Item[] {
  const items = new Map<string, number>();

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

  const resolveIdent = (ident: Identifier): Resolution => {
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

    if (BUILTIN_SET.has(ident.name)) {
      return { kind: "builtin", name: ident.name as BuiltinName };
    }

    throw new CompilerError(`cannot find ${ident.name}`, ident.span);
  };

  const blockLocals: LocalInfo[][] = [];

  const resolver: Folder = {
    ...DEFAULT_FOLDER,
    item(item) {
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
          };
        }
        case "mod": {
          if (item.node.modKind.kind === "inline") {
            const contents = resolveModule(cx, item.node.modKind.contents);
            return {
              ...item,
              kind: "mod",
              node: { ...item.node, modKind: { kind: "inline", contents } },
            };
          }
          break;
        }
      }

      return superFoldItem(item, this);
    },
    expr(expr) {
      switch (expr.kind) {
        case "block": {
          const prevScopeLength = scopes.length;
          blockLocals.push([]);

          const exprs = expr.exprs.map<Expr>((inner) => this.expr(inner));

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
          let rhs = this.expr(expr.rhs);
          let type = expr.type && this.type(expr.type);

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
          if (expr.lhs.kind === "ident") {
            // If the lhs is a module we need to convert this into a path.
            const res = resolveIdent(expr.lhs.value);
            if (res.kind === "item") {
              const module = unwrap(cx.ast.itemsById.get(res.id));
              if (module.kind === "mod") {
                if (typeof expr.field.value === "number") {
                  throw new CompilerError(
                    "module contents cannot be indexed with a number",
                    expr.field.span
                  );
                }

                const pathResItem = resolveModItem(
                  cx,
                  module.node,
                  module.id,
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
                  segments: [expr.lhs.value.name, expr.field.value],
                  res: pathRes,
                  span: spanMerge(expr.lhs.span, expr.field.span),
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
  };

  return contents.map((item) => resolver.item(item));
}
