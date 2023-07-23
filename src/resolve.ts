import {
  Ast,
  DEFAULT_FOLDER,
  Folder,
  Identifier,
  Resolution,
  fold_ast,
  super_fold_expr,
  super_fold_item,
} from "./ast";
import { CompilerError } from "./error";

const BUILTINS = new Set<string>(["print", "String"]);

export function resolve(ast: Ast): Ast {
  const items = new Map<string, number>();

  for (let i = 0; i < ast.length; i++) {
    const item = ast[i];
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
        `Scopes corrupted, wanted to pop ${name} but popped ${popped}`
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

    if (BUILTINS.has(ident.name)) {
      return { kind: "builtin" };
    }
    
    throw new CompilerError(`cannot find ${ident.name}`, ident.span);
  };

  const resolver: Folder = {
    ...DEFAULT_FOLDER,
    item(item) {
      switch (item.kind) {
        case "function": {
          const args = item.node.args.map(({ name, span, type }) => ({
            name,
            span,
            type: this.type(type),
          }));
          const returnType =
            item.node.returnType && this.type(item.node.returnType);

          item.node.args.forEach(({ name }) => scopes.push(name));
          const body = super_fold_expr(item.node.body, this);
          item.node.args.forEach(({ name }) => popScope(name));

          return {
            kind: "function",
            span: item.span,
            node: {
              name: item.node.name,
              args,
              returnType,
              body,
            },
          };
        }
      }

      return super_fold_item(item, this);
    },
    expr(expr) {
      if (expr.kind === "let") {
        const rhs = this.expr(expr.rhs);
        const type = expr.type && this.type(expr.type);
        scopes.push(expr.name);
        const after = this.expr(expr.after);
        popScope(expr.name);

        return {
          kind: "let",
          name: expr.name,
          rhs,
          after,
          type,
          span: expr.span,
        };
      }

      return super_fold_expr(expr, this);
    },
    ident(ident) {
      const res = resolveIdent(ident);
      return { name: ident.name, span: ident.span, res };
    },
  };

  const resolved = fold_ast(ast, resolver);

  return resolved;
}
