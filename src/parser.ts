import {
  ARITH_FACTOR_KINDS,
  ARITH_TERM_KINDS,
  BinaryKind,
  COMPARISON_KINDS,
  Expr,
  FunctionDef,
  Item,
  LOGICAL_KINDS,
  UNARY_KINDS,
  UnaryKind,
} from "./ast";
import { CompilerError, todo } from "./error";
import { BaseToken, Token, TokenIdent } from "./lexer";

type Parser<T> = (t: Token[]) => [Token[], T];

export function parse(t: Token[]): Item[] {
  const items: Item[] = [];

  while (t.length > 0) {
    let item;
    [t, item] = parseItem(t);
    items.push(item);
  }

  return items;
}

function parseItem(t: Token[]): [Token[], Item] {
  let tok;
  [t, tok] = next(t);
  if (tok.kind === "function") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");

    [t] = expectNext(t, "(");
    [t] = expectNext(t, ")");
    [t] = expectNext(t, "=");

    let body;
    [t, body] = parseExpr(t);

    [t] = expectNext(t, ";");

    const def: FunctionDef = {
      name: name.ident,
      args: [],
      body,
    };

    return [t, { kind: "function", node: def, span: tok.span }];
  } else {
    unexpectedToken(tok);
  }
}

function parseExpr(t: Token[]): [Token[], Expr] {
  /*
  EXPR = { "let" NAME "=" EXPR "in" EXPR | COMPARISON }

  // The precende here is pretty arbitrary since we forbid mixing of operators
  // with different precedence classes anyways.
  COMPARISON = LOGICAL { ( ">" | "<" | "==" | "<=" | ">=" | "!=" ) COMPARISON }
  LOGICAL = ARITH_TERM { ( "&" | "|" ) LOGICAL }

  // Here it matters though.
  ARITH_TERM = ATOM { ( "+" | "-" ) ARITH_TERM }
  ARITH_FACTOR = UNARY { ( "*" | "/" ) ARITH_FACTOR }

  UNARY = { "!" | "-" } CALL

  CALL = ATOM { "(" EXPR_LIST ")" }

  ATOM = "(" { EXPR ";" } EXPR ")" | IDENT | LITERAL | EMPTY
  EMPTY =
  EXPR_LIST = { EXPR { "," EXPR } { "," } }
  */
  const [, peak] = next(t);

  if (peak.kind === "let") {
    [t] = next(t);
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");
    expectNext(t, "=");
    let rhs;
    [t, rhs] = parseExpr(t);
    expectNext(t, "in");
    let after;
    [t, after] = parseExpr(t);

    return [t, { kind: "let", name: name.ident, rhs, after, span: t[0].span }];
  }

  return parseExprComparison(t);
}

function mkParserExprBinary(
  lower: Parser<Expr>,
  kinds: string[]
): Parser<Expr> {
  function parser(t: Token[]): [Token[], Expr] {
    let lhs;
    [t, lhs] = lower(t);

    const [, peak] = next(t);
    if (kinds.includes(peak.kind)) {
      [t] = next(t);
      let rhs;
      [t, rhs] = parser(t);
      const span = peak.span;
      return [
        t,
        { kind: "binary", binaryKind: peak.kind as BinaryKind, lhs, rhs, span },
      ];
    }

    return [t, lhs];
  }

  return parser;
}

const parseExprArithFactor = mkParserExprBinary(
  parseExprUnary,
  ARITH_FACTOR_KINDS
);

const parseExprArithTerm = mkParserExprBinary(
  parseExprArithFactor,
  ARITH_TERM_KINDS
);

const parseExprLogical = mkParserExprBinary(parseExprArithTerm, LOGICAL_KINDS);

const parseExprComparison = mkParserExprBinary(
  parseExprLogical,
  COMPARISON_KINDS
);

function parseExprUnary(t: Token[]): [Token[], Expr] {
  const [, peak] = next(t);
  if (peak.kind in UNARY_KINDS) {
    let rhs;
    [t, rhs] = parseExprUnary(t);
    return [
      t,
      {
        kind: "unary",
        unaryKind: peak.kind as UnaryKind,
        rhs,
        span: peak.span,
      },
    ];
  }

  return parseExprCall(t);
}
function parseExprCall(t: Token[]): [Token[], Expr] {
  let lhs: Expr;
  [t, lhs] = parseExprAtom(t);

  while (next(t)[1].kind === "(") {
    let popen;
    [t, popen] = next(t);
    const args = [];
    while (next(t)[1].kind !== ")") {
      let arg;
      [t, arg] = parseExpr(t);
      args.push(arg);
      // TODO i think this is incorrect
      [t] = eat(t, ",");
    }
    [t] = expectNext(t, ")");

    lhs = { kind: "call", span: popen.span, lhs, args };
  }

  return [t, lhs];
}

function parseExprAtom(startT: Token[]): [Token[], Expr] {
  let [t, tok] = next(startT);

  if (tok.kind === "(") {
    let expr: Expr;
    [t, expr] = parseExpr(t);

    const exprs = [expr];
    while (next(t)[1].kind !== ")") {
      [t] = expectNext(t, ";");
      [t, expr] = parseExpr(t);
      exprs.push(expr);
    }
    [t] = expectNext(t, ")");

    return [t, { kind: "block", span: tok.span, exprs }];
  }

  if (tok.kind === "lit_string") {
    return [
      t,
      {
        kind: "literal",
        span: tok.span,
        value: { kind: "str", value: tok.value },
      },
    ];
  }

  if (tok.kind === "lit_int") {
    return [
      t,
      {
        kind: "literal",
        span: tok.span,
        value: { kind: "int", value: tok.value },
      },
    ];
  }

  if (tok.kind === "identifier") {
    return [t, { kind: "ident", span: tok.span, value: tok.ident }];
  }

  // Parse nothing at all.
  return [startT, { kind: "empty", span: tok.span }];
}

// helpers

function eat<T extends BaseToken>(
  t: Token[],
  kind: T["kind"]
): [Token[], T | undefined] {
  const [tnext, tok] = next(t);
  if (tok.kind === kind) {
    return [tnext, tok as unknown as T];
  }
  return [t, undefined];
}

function expectNext<T extends BaseToken>(
  t: Token[],
  kind: T["kind"]
): [Token[], T] {
  let tok;
  [t, tok] = next(t);
  const token = expectToken(kind, tok);
  return [t, token];
}

function next(t: Token[]): [Token[], Token] {
  const [rest, next] = maybeNextT(t);
  if (!next) {
    throw new CompilerError("unexpected end of file", {
      start: Number.MAX_SAFE_INTEGER,
      end: Number.MAX_SAFE_INTEGER,
    });
  }
  return [rest, next];
}

function maybeNextT(t: Token[]): [Token[], Token | undefined] {
  const next = t[0];
  const rest = t.slice(1);
  return [rest, next];
}

function unexpectedToken(token: Token): never {
  throw new CompilerError("unexpected token", token.span);
}

function expectToken<T extends BaseToken>(kind: T["kind"], token: Token): T {
  if (token.kind !== kind) {
    throw new CompilerError(
      `expected ${kind}, found ${token.kind}`,
      token.span
    );
  }
  return token as unknown as T;
}
