import {
  Folder,
  Ident,
  Item,
  Pkg,
  Typecked,
  foldAst,
  mkDefaultFolder,
  superFoldExpr,
  superFoldItem,
} from "../ast";
import { GlobalContext } from "../context";
import { CompilerError } from "../error";

type LintContext = {
  locals: LocalUsed[];
};

type LocalUsed = {
  ident: Ident;
  used: boolean;
  isParam: boolean;
};

export function lintProgram(gcx: GlobalContext, ast: Pkg<Typecked>): void {
  const cx: LintContext = {
    locals: [],
  };

  const checker: Folder<Typecked, Typecked> = {
    ...mkDefaultFolder(),
    itemInner(item: Item<Typecked>): Item<Typecked> {
      switch (item.kind) {
        case "function": {
          const prev = cx.locals;
          cx.locals = [
            ...item.params.map((param) => ({
              ident: param.ident,
              used: false,
              isParam: true,
            })),
          ];
          superFoldItem(item, this);
          checkLocalsUsed(gcx, 0, cx);
          cx.locals = prev;
          return item;
        }
      }
      return superFoldItem(item, this);
    },
    expr(expr) {
      switch (expr.kind) {
        case "let": {
          superFoldExpr(expr, this);
          cx.locals.push({ ident: expr.name, used: false, isParam: false });
          return expr;
        }
        case "block": {
          const prevLocalsLen = cx.locals.length;
          superFoldExpr(expr, this);
          checkLocalsUsed(gcx, prevLocalsLen, cx);
          cx.locals.length = prevLocalsLen;

          return expr;
        }
        case "ident": {
          const { res } = expr.value;
          switch (res.kind) {
            case "local": {
              cx.locals[cx.locals.length - 1 - res.index].used = true;
              break;
            }
          }
          return superFoldExpr(expr, this);
        }
      }

      return superFoldExpr(expr, this);
    },
    ident(ident) {
      return ident;
    },
    type(type) {
      return type;
    },
  };

  foldAst(ast, checker);
}

function checkLocalsUsed(
  gcx: GlobalContext,
  prevLength: number,
  cx: LintContext,
) {
  for (let i = prevLength; i < cx.locals.length; i++) {
    const local = cx.locals[i];
    if (!local.used && !local.ident.name.startsWith("_")) {
      const kind = local.isParam ? "function parameter" : "variable";
      gcx.error.warn(
        new CompilerError(
          `unused ${kind}: \`${local.ident.name}\``,
          local.ident.span,
        ),
      );
    }
  }
}
