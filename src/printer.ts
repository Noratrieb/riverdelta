import { Expr, FunctionDef, Item } from "./ast";

export function printAst(ast: Item[]): string {
  return ast.map(printItem).join("\n");
}

function printItem(item: Item): string {
  switch (item.kind) {
    case "function": {
      return printFunction(item.node);
    }
  }
}

function printFunction(func: FunctionDef): string {
  const args = func.args.map(({ name }) => name).join(", ");
  return `function ${func.name}(${args}) = ${printExpr(func.body, 0)}`;
}

function printExpr(expr: Expr, indent: number): string {
  switch (expr.kind) {
    case "empty": {
      return "";
    }
    case "let": {
      return `let ${expr.name} = ${printExpr(expr.rhs, 1)} in${linebreak(
        indent
      )}`;
    }
    case "block": {
      const exprs = expr.exprs.map((expr) => printExpr(expr, indent + 1));

      if (exprs.length === 1) {
        return `(${exprs[0]})`;
      }
      const shortExprs =
        exprs.map((s) => s.length).reduce((a, b) => a + b, 0) < 40;
      if (shortExprs) {
        const alreadyHasTrailingSpace = expr.exprs[exprs.length - 1]?.kind === "empty";
        const trailingSpace = alreadyHasTrailingSpace ? "" : " ";
        return `( ${exprs.join("; ")}${trailingSpace})`;
      } else {
        const joiner = `;${linebreak(indent + 1)}`;
        return (
          `(${linebreak(indent + 1)}` +
          `${exprs.join(joiner)}` +
          `${linebreak(indent)})`
        );
      }
    }
    case "literal": {
      switch (expr.value.kind) {
        case "str": {
          return `"${expr.value.value}"`;
        }
        case "int": {
          return `${expr.value.value}`;
        }
      }
    }
    case "ident": {
      return expr.value;
    }
    case "binary": {
      return `${printExpr(expr.lhs, indent)} ${expr.binaryKind} ${printExpr(
        expr.rhs,
        indent
      )}`;
    }
    case "unary": {
      return `${expr.unaryKind}${printExpr(expr.rhs, indent)}`;
    }
    case "call": {
      const args = expr.args.map((arg) => printExpr(arg, indent + 1));
      const shortArgs =
        args.map((s) => s.length).reduce((a, b) => a + b, 0) < 40;
      if (shortArgs) {
        return `${printExpr(expr.lhs, indent)}(${args.join(", ")})`;
      } else {
        return (
          `${printExpr(expr.lhs, indent)}(${linebreak(indent + 1)}` +
          `${args.join(linebreak(indent + 1))}` +
          `${linebreak(indent)})`
        );
      }
    }
  }
}

function linebreak(indent: number): string {
  return `\n${ind(indent)}`;
}

function ind(indent: number): string {
  return "  ".repeat(indent * 2);
}
