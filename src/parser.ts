import {
  ARITH_FACTOR_KINDS,
  ARITH_TERM_KINDS,
  Ast,
  BinaryKind,
  COMPARISON_KINDS,
  DEFAULT_FOLDER,
  Expr,
  ExprLoop,
  ExprStructLiteral,
  FieldDef,
  Folder,
  FunctionArg,
  FunctionDef,
  Identifier,
  ImportDef,
  Item,
  LOGICAL_KINDS,
  ModItem,
  Type,
  TypeDef,
  UNARY_KINDS,
  UnaryKind,
  binaryExprPrecedenceClass,
  foldAst,
  superFoldExpr,
  superFoldItem,
} from "./ast";
import { CompilerError, Span, spanMerge } from "./error";
import { BaseToken, Token, TokenIdent, TokenLitString } from "./lexer";
import { Ids } from "./utils";

type Parser<T> = (t: Token[]) => [Token[], T];

export function parse(t: Token[]): Ast {
  const items: Item[] = [];

  while (t.length > 0) {
    let item;
    [t, item] = parseItem(t);
    items.push(item);
  }

  const ast = assignIds(items);

  validateAst(ast);

  return ast;
}

function parseItem(t: Token[]): [Token[], Item] {
  let tok;
  [t, tok] = next(t);
  if (tok.kind === "function") {
    let sig;
    [t, sig] = parseFunctionSig(t);

    [t] = expectNext(t, "=");

    let body;
    [t, body] = parseExpr(t);

    [t] = expectNext(t, ";");

    const def: FunctionDef = {
      ...sig,
      body,
    };

    return [
      t,
      {
        kind: "function",
        node: def,
        span: tok.span,
        // Assigned later.
        id: 0,
      },
    ];
  } else if (tok.kind === "type") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");
    [t] = expectNext(t, "=");
    [t] = expectNext(t, "{");

    let fields;
    [t, fields] = parseCommaSeparatedList<FieldDef>(t, "}", (t) => {
      let name;
      [t, name] = expectNext<TokenIdent>(t, "identifier");
      [t] = expectNext(t, ":");
      let type;
      [t, type] = parseType(t);
      return [
        t,
        {
          name: {
            name: name.ident,
            span: name.span,
          },
          type,
        },
      ];
    });

    [t] = expectNext(t, ";");

    const def: TypeDef = {
      name: name.ident,
      fields,
    };

    return [t, { kind: "type", node: def, span: name.span, id: 0 }];
  } else if (tok.kind === "import") {
    [t] = expectNext(t, "(");
    let module;
    [t, module] = expectNext<TokenLitString>(t, "lit_string");
    let func;
    [t, func] = expectNext<TokenLitString>(t, "lit_string");
    [t] = expectNext(t, ")");

    let sig;
    [t, sig] = parseFunctionSig(t);

    [t] = expectNext(t, ";");

    const def: ImportDef = {
      module: { kind: "str", value: module.value, span: module.span },
      func: { kind: "str", value: func.value, span: func.span },
      ...sig,
    };

    return [t, { kind: "import", node: def, span: tok.span, id: 0 }];
  } else if (tok.kind === "extern") {
    [t] = expectNext(t, "mod");
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");

    const node: ModItem = {
      name: name.ident,
      modKind: { kind: "extern" },
    };

    [t] = expectNext(t, ";");

    return [t, { kind: "mod", node, span: name.span, id: 0 }];
  } else if (tok.kind === "mod") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");

    [t] = expectNext(t, "(");

    const contents: Item[] = [];

    while (next(t)[1].kind !== ")") {
      let item;
      [t, item] = parseItem(t);

      contents.push(item);
    }

    [t] = expectNext(t, ")");
    [t] = expectNext(t, ";");

    const node: ModItem = {
      name: name.ident,
      modKind: { kind: "inline", contents },
    };

    return [t, { kind: "mod", node, span: name.span, id: 0 }];
  } else {
    unexpectedToken(tok, "item");
  }
}

type FunctionSig = {
  name: string;
  params: FunctionArg[];
  returnType?: Type;
};

function parseFunctionSig(t: Token[]): [Token[], FunctionSig] {
  let name;
  [t, name] = expectNext<TokenIdent>(t, "identifier");

  [t] = expectNext(t, "(");

  let params: FunctionArg[];
  [t, params] = parseCommaSeparatedList(t, ")", (t) => {
    let name;
    [t, name] = expectNext<TokenIdent & { span: Span }>(t, "identifier");
    [t] = expectNext(t, ":");
    let type;
    [t, type] = parseType(t);

    return [t, { name: name.ident, type, span: name.span }];
  });

  let colon;
  let returnType = undefined;
  [t, colon] = eat(t, ":");
  if (colon) {
    [t, returnType] = parseType(t);
  }

  return [t, { name: name.ident, params, returnType }];
}

function parseExpr(t: Token[]): [Token[], Expr] {
  /*
  EXPR = ASSIGNMENT

  LET = "let" NAME { ":" TYPE } "=" EXPR "in" EXPR
  IF = "if" EXPR "then" EXPR { "else" EXPR }
  LOOP = "loop" EXPR
  BREAK = "break"

  ASSIGNMENT = COMPARISON { "=" ASSIGNMENT }

  // The precende here is pretty arbitrary since we forbid mixing of operators
  // with different precedence classes anyways.
  COMPARISON = LOGICAL { ( ">" | "<" | "==" | "<=" | ">=" | "!=" ) COMPARISON }
  LOGICAL = ARITH_TERM { ( "&" | "|" ) LOGICAL }

  // Here it matters though.
  ARITH_TERM = ATOM { ( "+" | "-" ) ARITH_TERM }
  ARITH_FACTOR = UNARY { ( "*" | "/" ) ARITH_FACTOR }

  UNARY = { "!" | "-" } CALL

  CALL = ATOM { ( "(" EXPR_LIST ")" ) | ( "." ( IDENT | NUMBER ) ) }

  ATOM = "(" { EXPR ";" | "," } EXPR ")" | IDENT { STRUCT_INIT } | LITERAL | EMPTY | LET | IF | LOOP | BREAK
  EMPTY =
  STRUCT_INIT = "{" { NAME ":" EXPR } { "," NAME ":" EXPR } { "," } "}"
  EXPR_LIST = { EXPR { "," EXPR } { "," } }
  */
  return parseExprAssignment(t);
}

function mkBinaryExpr(lhs: Expr, rhs: Expr, span: Span, kind: string): Expr {
  return { kind: "binary", binaryKind: kind as BinaryKind, lhs, rhs, span };
}

function mkParserExprBinary(
  lower: Parser<Expr>,
  kinds: string[],
  mkExpr = mkBinaryExpr
): Parser<Expr> {
  function parser(t: Token[]): [Token[], Expr] {
    let lhs;
    [t, lhs] = lower(t);

    const [, peek] = next(t);
    if (kinds.includes(peek.kind)) {
      [t] = next(t);
      let rhs;
      [t, rhs] = parser(t);
      const span = spanMerge(lhs.span, rhs.span);
      return [t, mkExpr(lhs, rhs, span, peek.kind)];
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

const parseExprAssignment = mkParserExprBinary(
  parseExprComparison,
  ["="],
  (lhs, rhs, span) => ({ kind: "assign", lhs, rhs, span })
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

  while (next(t)[1].kind === "(" || next(t)[1].kind === ".") {
    let tok;
    [t, tok] = next(t);

    if (tok.kind === "(") {
      let args;
      [t, args] = parseCommaSeparatedList(t, ")", parseExpr);

      lhs = { kind: "call", span: tok.span, lhs, args };
    } else if (tok.kind === ".") {
      let access;
      [t, access] = next(t);
      let value;
      if (access.kind === "identifier") {
        value = access.ident;
      } else if (access.kind === "lit_int") {
        value = access.value;
      } else {
        unexpectedToken(access, "identifier or integer");
      }

      lhs = {
        kind: "fieldAccess",
        lhs,
        field: { span: access.span, value },
        span: spanMerge(lhs.span, access.span),
      };
    }
  }

  return [t, lhs];
}

function parseExprAtom(startT: Token[]): [Token[], Expr] {
  let [t, tok] = next(startT);
  const span = tok.span;

  if (tok.kind === "(") {
    let expr: Expr;
    [t, expr] = parseExpr(t);

    // This could be a block or a tuple literal. We can only know after
    // parsing the first expression and looking at the delimiter.

    let peek;
    [, peek] = next(t);
    // It's a single element, which we interpret as a block.
    // `(0,)` is the one elem tuple.
    if (peek.kind === ")") {
      [t] = expectNext(t, ")");
      return [t, { kind: "block", span, exprs: [expr] }];
    }
    // It's a block.
    if (peek.kind === ";") {
      const exprs = [expr];
      while (next(t)[1].kind !== ")") {
        [t] = expectNext(t, ";");
        [t, expr] = parseExpr(t);
        exprs.push(expr);
      }
      [t] = expectNext(t, ")");

      return [t, { kind: "block", span, exprs }];
    }
    // It's a tuple.
    if (peek.kind === ",") {
      [t] = expectNext(t, ",");
      let rest;
      [t, rest] = parseCommaSeparatedList(t, ")", parseExpr);

      return [t, { kind: "tupleLiteral", span, fields: [expr, ...rest] }];
    }
    unexpectedToken(peek, "`,`, `;` or `)`");
  }

  if (tok.kind === "lit_string") {
    return [
      t,
      {
        kind: "literal",
        span,
        value: { kind: "str", value: tok.value, span: tok.span },
      },
    ];
  }

  if (tok.kind === "lit_int") {
    return [
      t,
      {
        kind: "literal",
        span,
        value: { kind: "int", value: tok.value, type: tok.type },
      },
    ];
  }

  if (tok.kind === "identifier") {
    if (maybeNextT(t)[1]?.kind === "{") {
      let fields;
      [t, fields] = parseStructInit(t);
      return [
        t,
        {
          kind: "structLiteral",
          name: { name: tok.ident, span },
          fields,
          span,
        },
      ];
    }

    return [
      t,
      {
        kind: "ident",
        span,
        value: { name: tok.ident, span },
      },
    ];
  }

  if (tok.kind === "let") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");

    let type = undefined;
    let colon;
    [t, colon] = eat(t, ":");
    if (colon) {
      [t, type] = parseType(t);
    }

    [t] = expectNext(t, "=");
    let rhs;
    [t, rhs] = parseExpr(t);

    const nameIdent: Identifier = { name: name.ident, span: name.span };

    return [
      t,
      {
        kind: "let",
        name: nameIdent,
        type,
        rhs,
        span: name.span,
      },
    ];
  }

  if (tok.kind === "if") {
    let cond;
    [t, cond] = parseExpr(t);

    [t] = expectNext(t, "then");
    let then;
    [t, then] = parseExpr(t);

    let elseTok;
    [t, elseTok] = eat(t, "else");
    let elsePart = undefined;
    if (elseTok) {
      [t, elsePart] = parseExpr(t);
    }

    return [t, { kind: "if", cond, then, else: elsePart, span: tok.span }];
  }

  if (tok.kind === "loop") {
    let body;
    [t, body] = parseExpr(t);
    return [t, { kind: "loop", body, span: tok.span, loopId: 0 }];
  }

  if (tok.kind === "break") {
    return [t, { kind: "break", span: tok.span }];
  }

  // Parse nothing at all.
  return [startT, { kind: "empty", span }];
}

function parseStructInit(t: Token[]): [Token[], ExprStructLiteral["fields"]] {
  [t] = expectNext(t, "{");

  let fields;
  [t, fields] = parseCommaSeparatedList<[Identifier, Expr]>(t, "}", (t) => {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");
    [t] = expectNext(t, ":");
    let expr;
    [t, expr] = parseExpr(t);

    return [t, [{ name: name.ident, span: name.span }, expr]];
  });

  return [t, fields];
}

function parseType(t: Token[]): [Token[], Type] {
  let tok;
  [t, tok] = next(t);
  const span = tok.span;

  switch (tok.kind) {
    case "!": {
      return [t, { kind: "never", span }];
    }
    case "identifier": {
      return [
        t,
        {
          kind: "ident",
          value: { name: tok.ident, span },
          span,
        },
      ];
    }
    case "[": {
      let elem;
      [t, elem] = parseType(t);
      [t] = expectNext(t, "]");
      return [t, { kind: "list", elem, span }];
    }
    case "(": {
      // `()` is a the unit type, an empty tuple.
      // `(T)` is just `T`
      // `(T,)` is a tuple
      if (next(t)[1]?.kind === ")") {
        [t] = next(t);
        return [t, { kind: "tuple", elems: [], span }];
      }
      let head;
      [t, head] = parseType(t);

      if (next(t)[1]?.kind === ")") {
        [t] = next(t);
        // Just a type inside parens, not a tuple. `(T,)` is a tuple.
        return [t, head];
      }

      [t] = expectNext(t, ",");

      let tail;
      [t, tail] = parseCommaSeparatedList(t, ")", parseType);

      return [t, { kind: "tuple", elems: [head, ...tail], span }];
    }
    default: {
      throw new CompilerError(
        `unexpected token: \`${tok.kind}\`, expected type`,
        span
      );
    }
  }
}

// helpers

function parseCommaSeparatedList<R>(
  t: Token[],
  terminator: Token["kind"],
  parser: Parser<R>
): [Token[], R[]] {
  const items: R[] = [];

  // () | (a) | (a,) | (a, b)

  while (true) {
    if (next(t)[1]?.kind === terminator) {
      break;
    }

    let nextValue;
    [t, nextValue] = parser(t);

    items.push(nextValue);

    let comma;
    [t, comma] = eat(t, ",");
    if (!comma) {
      // No comma? Fine, you don't like trailing commas.
      // But this better be the end.
      if (next(t)[1]?.kind !== terminator) {
        unexpectedToken(next(t)[1], `, or ${terminator}`);
      }
      break;
    }
  }

  [t] = expectNext(t, terminator);

  return [t, items];
}

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
): [Token[], T & Token] {
  let tok;
  [t, tok] = next(t);
  if (tok.kind !== kind) {
    throw new CompilerError(
      `expected \`${kind}\`, found \`${tok.kind}\``,
      tok.span
    );
  }
  return [t, tok as unknown as T & Token];
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

function unexpectedToken(token: Token, expected: string): never {
  throw new CompilerError(`unexpected token, expected ${expected}`, token.span);
}

function validateAst(ast: Ast) {
  const seenItemIds = new Set();

  const validator: Folder = {
    ...DEFAULT_FOLDER,
    item(item: Item): Item {
      if (seenItemIds.has(item.id)) {
        throw new Error(`duplicate item id: ${item.id} for ${item.node.name}`);
      }
      seenItemIds.add(item.id);
      return superFoldItem(item, this);
    },
    expr(expr: Expr): Expr {
      if (expr.kind === "block") {
        expr.exprs.forEach((inner) => {
          if (inner.kind === "let") {
            this.expr(inner.rhs);
            if (inner.type) {
              this.type(inner.type);
            }
          } else {
            this.expr(inner);
          }
        });
        return expr;
      } else if (expr.kind === "let") {
        throw new CompilerError("let is only allowed in blocks", expr.span);
      } else if (expr.kind === "binary") {
        const checkPrecedence = (inner: Expr, side: string) => {
          if (inner.kind === "binary") {
            const ourClass = binaryExprPrecedenceClass(expr.binaryKind);
            const innerClass = binaryExprPrecedenceClass(inner.binaryKind);

            if (ourClass !== innerClass) {
              throw new CompilerError(
                `mixing operators without parentheses is not allowed. ${side} is ${inner.binaryKind}, which is different from ${expr.binaryKind}`,
                expr.span
              );
            }
          }
        };

        checkPrecedence(expr.lhs, "left");
        checkPrecedence(expr.rhs, "right");

        return superFoldExpr(expr, this);
      } else {
        return superFoldExpr(expr, this);
      }
    },
  };

  foldAst(ast, validator);
}

function assignIds(rootItems: Item[]): Ast {
  const itemId = new Ids();
  const loopId = new Ids();

  const ast: Ast = {
    rootItems,
    itemsById: new Map(),
  };

  const assigner: Folder = {
    ...DEFAULT_FOLDER,
    item(item: Item): Item {
      const id = itemId.next();
      ast.itemsById.set(id, item);
      return { ...superFoldItem(item, this), id };
    },
    expr(expr: Expr): Expr {
      if (expr.kind === "loop") {
        return {
          ...(superFoldExpr(expr, this) as ExprLoop & Expr),
          loopId: loopId.next(),
        };
      }
      return superFoldExpr(expr, this);
    },
  };

  return foldAst(ast, assigner);
}
