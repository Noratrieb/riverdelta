import {
  AnyPhase,
  Crate,
  Expr,
  ItemFunction,
  IdentWithRes,
  ItemImport,
  Item,
  ItemMod,
  Resolution,
  StringLiteral,
  Ty,
  Type,
  ItemType,
  tyIsUnit,
} from "./ast";

export function printAst(ast: Crate<AnyPhase>): string {
  return ast.rootItems.map(printItem).join("\n");
}

function printStringLiteral(lit: StringLiteral): string {
  return `"${lit.value.replace("\n", "\\n")}"`;
}

function printItem(item: Item<AnyPhase>): string {
  const id = `/*${item.id.toString()}*/ `;

  switch (item.kind) {
    case "function": {
      return id + printFunction(item);
    }
    case "type": {
      return id + printTypeDef(item);
    }
    case "import": {
      return id + printImportDef(item);
    }
    case "mod": {
      return id + printMod(item);
    }
    case "extern": {
      return id + `extern mod ${item.name};`;
    }
    case "global": {
      return (
        id +
        `global ${item.name}: ${printType(item.type)} = ${printExpr(
          item.init,
          0,
        )};`
      );
    }
  }
}

function printFunction(func: ItemFunction<AnyPhase>): string {
  const args = func.params
    .map(({ name, type }) => `${name}: ${printType(type)}`)
    .join(", ");
  const ret = func.returnType ? `: ${printType(func.returnType)}` : "";
  return `function ${func.name}(${args})${ret} = ${printExpr(func.body, 0)};`;
}

function printTypeDef(type: ItemType<AnyPhase>): string {
  switch (type.type.kind) {
    case "struct": {
      const { fields } = type.type;

      const fieldStr = fields.map(
        ({ name, type }) => `${ind(1)}${name.name}: ${printType(type)},`,
      );
      const fieldPart =
        fields.length === 0 ? "{}" : `{\n${fieldStr.join("\n")}\n}`;

      return `type ${type.name} = ${fieldPart};`;
    }
    case "alias": {
      return `type ${type.name} = ${printType(type.type.type)}`;
    }
  }
}

function printImportDef(def: ItemImport<AnyPhase>): string {
  const args = def.params
    .map(({ name, type }) => `${name}: ${printType(type)}`)
    .join(", ");
  const ret = def.returnType ? `: ${printType(def.returnType)}` : "";

  return `import ${printStringLiteral(def.module)} ${printStringLiteral(
    def.func,
  )}(${args})${ret};`;
}

function printMod(mod: ItemMod<AnyPhase>): string {
  return `mod ${mod.name} (\n${mod.contents.map(printItem).join("\n  ")});`;
}

function printExpr(expr: Expr<AnyPhase>, indent: number): string {
  switch (expr.kind) {
    case "empty": {
      return "";
    }
    case "let": {
      const type = expr.type ? `: ${printType(expr.type)}` : "";

      return `let ${expr.name.name}${type} = ${printExpr(
        expr.rhs,
        indent + 1,
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
    case "path": {
      return `<${expr.segments.join(".")}>${printRes(expr.value.res)}`;
    }
    case "binary": {
      return `${printExpr(expr.lhs, indent)} ${expr.binaryKind} ${printExpr(
        expr.rhs,
        indent,
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
    case "fieldAccess": {
      return `${printExpr(expr.lhs, indent)}.${expr.field.value}`;
    }
    case "if": {
      const elsePart = expr.else
        ? ` else ${printExpr(expr.else, indent + 1)}`
        : "";
      return `if ${printExpr(expr.cond, indent + 1)} then ${printExpr(
        expr.then,
        indent + 1,
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
        .map(({ name, expr }) => `${name.name}: ${printExpr(expr, indent + 1)}`)
        .join(", ")} }`;
    }
    case "tupleLiteral": {
      return `(${expr.fields
        .map((expr) => printExpr(expr, indent))
        .join(", ")})`;
    }
  }
}

function printType(type: Type<AnyPhase>): string {
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

function printRes(res: Resolution): string {
  switch (res.kind) {
    case "local":
      return `#${res.index}`;
    case "item":
      return `#I${res.id.toString()}`;
    case "builtin": {
      return `#B`;
    }
  }
}

function printIdent(ident: IdentWithRes<AnyPhase>): string {
  const res = "res" in ident ? printRes(ident.res) : "";
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
      return ty._name;
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
