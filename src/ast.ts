import { Span } from "./error";

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

export type ExprKind =
  | { kind: "empty" }
  | { kind: "let"; name: string; type?: Type; rhs: Expr; after: Expr }
  | { kind: "block"; exprs: Expr[] }
  | {
      kind: "literal";
      value: Literal;
    }
  | {
      kind: "ident";
      value: string;
    }
  | {
      kind: "binary";
      binaryKind: BinaryKind;
      lhs: Expr;
      rhs: Expr;
    }
  | {
      kind: "unary";
      unaryKind: UnaryKind;
      rhs: Expr;
    }
  | {
      kind: "call";
      lhs: Expr;
      args: Expr[];
    }
  | {
      kind: "if";
      cond: Expr;
      then: Expr;
      else?: Expr;
    };

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
      value: string;
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
