import {
  Ast,
  Expr,
  FunctionDef,
  Identifier,
  ImportDef,
  Item,
  Resolution,
  StringLiteral,
  Ty,
  Type,
  TypeDef,
  tyIsUnit,
} from "./ast";

export function printAst(ast: Ast): string {
  return ast.items.map(printItem).join("\n");
}

function printStringLiteral(lit: StringLiteral): string {
  return `"${lit.value.replace("\n", "\\n")}"`;
}

function printItem(item: Item): string {
  switch (item.kind) {
    case "function": {
      return printFunction(item.node);
    }
    case "type": {
      return printTypeDef(item.node);
    }
    case "import": {
      return printImportDef(item.node);
    }
  }
}

function printFunction(func: FunctionDef): string {
  const args = func.params
    .map(({ name, type }) => `${name}: ${printType(type)}`)
    .join(", ");
  const ret = func.returnType ? `: ${printType(func.returnType)}` : "";
  return `function ${func.name}(${args})${ret} = ${printExpr(func.body, 0)};`;
}

function printTypeDef(type: TypeDef): string {
  const fields = type.fields.map(
    ({ name, type }) => `${ind(1)}${name.name}: ${printType(type)},`
  );

  const fieldPart =
    type.fields.length === 0 ? "{}" : `{\n${fields.join("\n")}\n}`;

  return `type ${type.name} = ${fieldPart};`;
}

function printImportDef(def: ImportDef): string {
  const args = def.params
    .map(({ name, type }) => `${name}: ${printType(type)}`)
    .join(", ");
  const ret = def.returnType ? `: ${printType(def.returnType)}` : "";

  return `import ${printStringLiteral(def.module)} ${printStringLiteral(
    def.func
  )}(${args})${ret};`;
}

function printExpr(expr: Expr, indent: number): string {
  switch (expr.kind) {
    case "empty": {
      return "";
    }
    case "let": {
      const type = expr.type ? `: ${printType(expr.type)}` : "";

      return `let ${expr.name.name}${type} = ${printExpr(
        expr.rhs,
        indent + 1
      )}`;
    }
    case "assign": {
      return `${printExpr(expr.lhs, indent)} = ${printExpr(expr.rhs, indent)}`;
    }
    case "block": {
      const exprs = expr.exprs.map((expr) => printExpr(expr, indent + 1));

      if (exprs.length === 1) {
        return `(${exprs[0]})`;
      }
      const shortExprs =
        exprs.map((s) => s.length).reduce((a, b) => a + b, 0) < 40;

      const alreadyHasTrailingSpace =
        expr.exprs[exprs.length - 1]?.kind === "empty";
      if (shortExprs) {
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
          return printStringLiteral(expr.value);
        }
        case "int": {
          return `${expr.value.value}_${expr.value.type}`;
        }
      }
    }
    case "ident": {
      return printIdent(expr.value);
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
    case "if": {
      const elsePart = expr.else
        ? ` else ${printExpr(expr.else, indent + 1)}`
        : "";
      return `if ${printExpr(expr.cond, indent + 1)} then ${printExpr(
        expr.then,
        indent + 1
      )}${elsePart}`;
    }
    case "loop": {
      return `loop ${printExpr(expr.body, indent)}`;
    }
    case "break": {
      const target = expr.target !== undefined ? `#${expr.target}` : "";
      return `break${target}`;
    }
    case "structLiteral": {
      return `${printIdent(expr.name)} { ${expr.fields
        .map(([name, expr]) => `${name.name}: ${printExpr(expr, indent + 1)}`)
        .join(", ")} }`;
    }
    case "tupleLiteral": {
      return `(${expr.fields
        .map((expr) => printExpr(expr, indent))
        .join(", ")})`;
    }
  }
}

function printType(type: Type): string {
  switch (type.kind) {
    case "ident":
      return printIdent(type.value);
    case "list":
      return `[${printType(type.elem)}]`;
    case "tuple":
      return `(${type.elems.map(printType).join(", ")})`;
    case "never":
      return "!";
  }
}

function printIdent(ident: Identifier): string {
  const printRes = (res: Resolution): string => {
    switch (res.kind) {
      case "local":
        return `#${res.index}`;
      case "item":
        return `#G${res.index}`;
      case "builtin": {
        return `#B`;
      }
    }
  };
  const res = ident.res ? printRes(ident.res) : "";
  return `${ident.name}${res}`;
}

export function printTy(ty: Ty): string {
  switch (ty.kind) {
    case "string": {
      return "String";
    }
    case "int": {
      return "Int";
    }
    case "i32": {
      return "I32";
    }
    case "bool": {
      return "Bool";
    }
    case "list": {
      return `[${printTy(ty.elem)}]`;
    }
    case "tuple": {
      return `(${ty.elems.map(printTy).join(", ")})`;
    }
    case "fn": {
      const ret = tyIsUnit(ty.returnTy) ? "" : `: ${printTy(ty.returnTy)}`;
      return `fn(${ty.params.map(printTy).join(", ")})${ret}`;
    }
    case "var": {
      return `?${ty.index}`;
    }
    case "struct": {
      return ty.name;
    }
    case "never": {
      return "!";
    }
  }
}

function linebreak(indent: number): string {
  return `\n${ind(indent)}`;
}

function ind(indent: number): string {
  return "  ".repeat(indent);
}
