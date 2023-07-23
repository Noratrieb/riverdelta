import { Span } from "./error";

export type Ast = Item[];

export type Identifier = {
  name: string;
  span: Span;
  res?: Resolution;
};

export type ItemKind = {
  kind: "function";
  node: FunctionDef;
};

export type Item = ItemKind & {
  span: Span;
};

export type FunctionDef = {
  name: string;
  args: FunctionArg[];
  body: Expr;
  returnType?: Type;
};

export type FunctionArg = {
  name: string;
  type: Type;
  span: Span;
};

export type ExprEmpty = { kind: "empty" };

export type ExprLet = {
  kind: "let";
  name: string;
  type?: Type;
  rhs: Expr;
  after: Expr;
};

export type ExprBlock = {
  kind: "block";
  exprs: Expr[];
};

export type ExprLiteral = {
  kind: "literal";
  value: Literal;
};

export type ExprIdent = {
  kind: "ident";
  value: Identifier;
};

export type ExprBinary = {
  kind: "binary";
  binaryKind: BinaryKind;
  lhs: Expr;
  rhs: Expr;
};

export type ExprUnary = {
  kind: "unary";
  unaryKind: UnaryKind;
  rhs: Expr;
};

export type ExprCall = {
  kind: "call";
  lhs: Expr;
  args: Expr[];
};

export type ExprIf = {
  kind: "if";
  cond: Expr;
  then: Expr;
  else?: Expr;
};

export type ExprKind =
  | ExprEmpty
  | ExprLet
  | ExprBlock
  | ExprLiteral
  | ExprIdent
  | ExprBinary
  | ExprUnary
  | ExprCall
  | ExprIf;

export type Expr = ExprKind & {
  span: Span;
};

export type Literal =
  | {
      kind: "str";
      value: string;
    }
  | {
      kind: "int";
      value: number;
    };

export type BinaryKind =
  | "+"
  | "-"
  | "*"
  | "/"
  | "&"
  | "|"
  | "<"
  | ">"
  | "=="
  | "<="
  | ">="
  | "!=";

export const COMPARISON_KINDS: BinaryKind[] = [
  ">",
  "<",
  "==",
  "<=",
  ">=",
  "!=",
];
export const LOGICAL_KINDS: BinaryKind[] = ["&", "|"];
export const ARITH_TERM_KINDS: BinaryKind[] = ["+", "-"];
export const ARITH_FACTOR_KINDS: BinaryKind[] = ["*", "/"];

const BINARY_KIND_PREC_CLASS = new Map<BinaryKind, number>([
  ["+", 0],
  ["-", 0],
  ["*", 0],
  ["/", 0],
  ["&", 1],
  ["|", 2],
  ["<", 3],
  [">", 4],
  ["==", 5],
  ["<=", 6],
  [">=", 7],
  ["!=", 8],
]);

export function binaryExprPrecedenceClass(k: BinaryKind): number {
  const cls = BINARY_KIND_PREC_CLASS.get(k);
  if (!cls) {
    throw new Error(`Invalid binary kind: ${k}`);
  }
  return cls;
}

export type UnaryKind = "!" | "-";
export const UNARY_KINDS: UnaryKind[] = ["!", "-"];

export type TypeKind =
  | {
      kind: "ident";
      value: Identifier;
    }
  | {
      kind: "list";
      elem: Type;
    }
  | {
      kind: "tuple";
      elems: Type[];
    };

export type Type = TypeKind & {
  span: Span;
};

export type Resolution =
  | {
      kind: "local";
      /**
       * The index of the local variable, from inside out.
       * ```
       * let a in let b in (a, b);
       *     ^        ^
       *     1        0
       * ```
       * When traversing resolutions, a stack of locals has to be kept.
       * It's similar to a De Bruijn index.
       */
      index: number;
    }
  | {
      kind: "item";
      /**
       * Items are numbered in the order they appear in.
       * Right now we only have one scope of items (global)
       * so this is enough.
       */
      index: number;
    }
  | {
      kind: "builtin";
    };

// folders

export type FoldFn<T> = (value: T) => T;

export type Folder = {
  item: FoldFn<Item>;
  expr: FoldFn<Expr>;
  ident: FoldFn<Identifier>;
  type: FoldFn<Type>;
};

export const DEFAULT_FOLDER: Folder = {
  item(item) {
    return super_fold_item(item, this);
  },
  expr(expr) {
    return super_fold_expr(expr, this);
  },
  ident(ident) {
    return ident;
  },
  type(type) {
    return super_fold_type(type, this);
  },
};

export function fold_ast(ast: Ast, folder: Folder): Ast {
  return ast.map((item) => folder.item(item));
}

export function super_fold_item(item: Item, folder: Folder): Item {
  switch (item.kind) {
    case "function": {
      const args = item.node.args.map(({ name, type, span }) => ({
        name,
        type: folder.type(type),
        span,
      }));

      return {
        kind: "function",
        span: item.span,
        node: {
          name: item.node.name,
          args,
          body: folder.expr(item.node.body),
          returnType: item.node.returnType && folder.type(item.node.returnType),
        },
      };
    }
  }
}

export function super_fold_expr(expr: Expr, folder: Folder): Expr {
  const span = expr.span;
  switch (expr.kind) {
    case "empty": {
      return { kind: "empty", span };
    }
    case "let": {
      return {
        kind: "let",
        name: expr.name,
        type: expr.type && folder.type(expr.type),
        rhs: folder.expr(expr.rhs),
        after: folder.expr(expr.after),
        span,
      };
    }
    case "block": {
      return {
        kind: "block",
        exprs: expr.exprs.map((expr) => folder.expr(expr)),
        span,
      };
    }
    case "literal": {
      return { kind: "literal", value: expr.value, span };
    }
    case "ident": {
      return { kind: "ident", value: folder.ident(expr.value), span };
    }
    case "binary": {
      return {
        kind: "binary",
        binaryKind: expr.binaryKind,
        lhs: folder.expr(expr.lhs),
        rhs: folder.expr(expr.rhs),
        span,
      };
    }
    case "unary": {
      return {
        kind: "unary",
        unaryKind: expr.unaryKind,
        rhs: folder.expr(expr.rhs),
        span,
      };
    }
    case "call": {
      return {
        kind: "call",
        lhs: folder.expr(expr.lhs),
        args: expr.args.map((expr) => folder.expr(expr)),
        span,
      };
    }
    case "if": {
      return {
        kind: "if",
        cond: folder.expr(expr.cond),
        then: folder.expr(expr.then),
        else: expr.else && folder.expr(expr.else),
        span,
      };
    }
  }
}

export function super_fold_type(type: Type, folder: Folder): Type {
  const span = type.span;
  switch (type.kind) {
    case "ident": {
      return {
        kind: "ident",
        value: folder.ident(type.value),
        span,
      };
    }
    case "list": {
      return {
        kind: "list",
        elem: folder.type(type.elem),
        span,
      };
    }
    case "tuple": {
      return {
        kind: "tuple",
        elems: type.elems.map((type) => folder.type(type)),
        span,
      };
    }
  }
}
