import { Span } from "./error";

export type ItemKind = {
    kind: "function",
    node: FunctionDef,
};

export type Item = ItemKind & {
    span: Span,
}

export type FunctionDef = {
    name: string,
    args: FunctionArg[], 
    body: Expr,
}

export type FunctionArg = {
    name: string,
    span: Span,
}

export type ExprKind = {
    kind: "lit_string",
    value: string,
} | {
    kind: "ident",
    value: string,
}

export type Expr = ExprKind & {
    span: Span,
}
