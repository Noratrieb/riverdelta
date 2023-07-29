import {
  Ast,
  BUILTINS,
  BuiltinName,
  DEFAULT_FOLDER,
  Expr,
  Folder,
  Identifier,
  LocalInfo,
  Resolution,
  foldAst,
  superFoldExpr,
  superFoldItem,
} from "./ast";
import { CompilerError } from "./error";

const BUILTIN_SET = new Set<string>(BUILTINS);

export function resolve(ast: Ast): Ast {
  const items = new Map<string, number>();

  for (let i = 0; i < ast.items.length; i++) {
    const item = ast.items[i];
    const existing = items.get(item.node.name);
    if (existing !== undefined) {
      throw new CompilerError(
        `item \`${item.node.name}\` has already been declared`,
        item.span
      );
    }
    items.set(item.node.name, i);
  }

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
        index: item,
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
      }

      return superFoldItem(item, this);
    },
    expr(expr) {
      if (expr.kind === "block") {
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
      } else if (expr.kind === "let") {
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
      } else {
        return superFoldExpr(expr, this);
      }
    },
    ident(ident) {
      const res = resolveIdent(ident);
      return { name: ident.name, span: ident.span, res };
    },
  };

  const resolved = foldAst(ast, resolver);

  return resolved;
}
