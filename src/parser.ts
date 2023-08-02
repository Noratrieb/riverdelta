import {
  ARITH_FACTOR_KINDS,
  ARITH_TERM_KINDS,
  Crate,
  BinaryKind,
  COMPARISON_KINDS,
  mkDefaultFolder,
  Expr,
  ExprLoop,
  ExprStructLiteral,
  FieldDef,
  Folder,
  FunctionArg,
  FunctionDef,
  Ident,
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
  Built,
  Parsed,
  ExternItem,
  ItemId,
  GlobalItem,
  StructLiteralField,
} from "./ast";
import { CompilerError, LoadedFile, Span } from "./error";
import {
  BaseToken,
  Token,
  TokenIdent,
  TokenLitString,
  tokenize,
} from "./lexer";
import { loadModuleFile } from "./loader";
import { ComplexMap, ComplexSet, Ids } from "./utils";

export type ParseState = { tokens: Token[]; file: LoadedFile };
type State = ParseState;

type Parser<T> = (t: State) => [State, T];

export function parse(
  packageName: string,
  t: State,
  crateId: number,
): Crate<Built> {
  const [, items] = parseItems(t);

  const ast: Crate<Built> = buildCrate(packageName, items, crateId, t.file);

  validateAst(ast);

  return ast;
}

function parseItems(t: State): [State, Item<Parsed>[]] {
  const items: Item<Parsed>[] = [];

  while (t.tokens.length > 0) {
    let item;
    [t, item] = parseItem(t);
    items.push(item);
  }

  return [t, items];
}

function parseItem(t: State): [State, Item<Parsed>] {
  let tok;
  [t, tok] = next(t);
  if (tok.kind === "function") {
    let sig;
    [t, sig] = parseFunctionSig(t);

    [t] = expectNext(t, "=");

    let body;
    [t, body] = parseExpr(t);

    [t] = expectNext(t, ";");

    const def: FunctionDef<Parsed> = {
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
        id: ItemId.dummy(),
      },
    ];
  } else if (tok.kind === "type") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");
    [t] = expectNext(t, "=");
    [t] = expectNext(t, "{");

    let fields;
    [t, fields] = parseCommaSeparatedList<FieldDef<Parsed>>(t, "}", (t) => {
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

    const def: TypeDef<Parsed> = {
      name: name.ident,
      fields,
    };

    return [
      t,
      { kind: "type", node: def, span: name.span, id: ItemId.dummy() },
    ];
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

    const def: ImportDef<Parsed> = {
      module: { kind: "str", value: module.value, span: module.span },
      func: { kind: "str", value: func.value, span: func.span },
      ...sig,
    };

    return [
      t,
      { kind: "import", node: def, span: tok.span, id: ItemId.dummy() },
    ];
  } else if (tok.kind === "extern") {
    [t] = expectNext(t, "mod");
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");

    const node: ExternItem = {
      name: name.ident,
    };

    [t] = expectNext(t, ";");

    return [t, { kind: "extern", node, span: name.span, id: ItemId.dummy() }];
  } else if (tok.kind === "mod") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");

    let contents: Item<Parsed>[] = [];

    let popen = undefined;
    [t, popen] = eat(t, "(");
    if (popen) {
      while (peekKind(t) !== ")") {
        let item;
        [t, item] = parseItem(t);

        contents.push(item);
      }

      [t] = expectNext(t, ")");
    } else {
      if (name.span.file.path === undefined) {
        throw new CompilerError(
          `no known source file for statement, cannot load file relative to it`,
          name.span,
        );
      }
      const file = loadModuleFile(name.span.file.path, name.ident, name.span);

      const tokens = tokenize(file);
      [, contents] = parseItems({ file, tokens });
    }

    [t] = expectNext(t, ";");

    const node: ModItem<Parsed> = {
      name: name.ident,
      contents,
    };

    return [t, { kind: "mod", node, span: name.span, id: ItemId.dummy() }];
  } else if (tok.kind === "global") {
    let name;
    [t, name] = expectNext<TokenIdent>(t, "identifier");
    [t] = expectNext(t, ":");
    let type;
    [t, type] = parseType(t);
    [t] = expectNext(t, "=");
    let init;
    [t, init] = parseExpr(t);
    [t] = expectNext(t, ";");

    const node: GlobalItem<Parsed> = {
      name: name.ident,
      type,
      init,
    };

    return [t, { kind: "global", node, span: name.span, id: ItemId.dummy() }];
  } else {
    unexpectedToken(tok, "item");
  }
}

type FunctionSig = {
  name: string;
  params: FunctionArg<Parsed>[];
  returnType?: Type<Parsed>;
};

function parseFunctionSig(t: State): [State, FunctionSig] {
  let name;
  [t, name] = expectNext<TokenIdent>(t, "identifier");

  [t] = expectNext(t, "(");

  let params: FunctionArg<Parsed>[];
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

function parseExpr(t: State): [State, Expr<Parsed>] {
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

function mkBinaryExpr(
  lhs: Expr<Parsed>,
  rhs: Expr<Parsed>,
  span: Span,
  kind: string,
): Expr<Parsed> {
  return { kind: "binary", binaryKind: kind as BinaryKind, lhs, rhs, span };
}

function mkParserExprBinary(
  lower: Parser<Expr<Parsed>>,
  kinds: string[],
  mkExpr = mkBinaryExpr,
): Parser<Expr<Parsed>> {
  function parser(t: State): [State, Expr<Parsed>] {
    let lhs;
    [t, lhs] = lower(t);

    const peek = peekKind(t);
    if (peek && kinds.includes(peek)) {
      let tok;
      [t, tok] = next(t);
      let rhs;
      [t, rhs] = parser(t);
      const span = lhs.span.merge(rhs.span);

      return [t, mkExpr(lhs, rhs, span, tok.kind)];
    }

    return [t, lhs];
  }

  return parser;
}

const parseExprArithFactor = mkParserExprBinary(
  parseExprUnary,
  ARITH_FACTOR_KINDS,
);

const parseExprArithTerm = mkParserExprBinary(
  parseExprArithFactor,
  ARITH_TERM_KINDS,
);

const parseExprLogical = mkParserExprBinary(parseExprArithTerm, LOGICAL_KINDS);

const parseExprComparison = mkParserExprBinary(
  parseExprLogical,
  COMPARISON_KINDS,
);

const parseExprAssignment = mkParserExprBinary(
  parseExprComparison,
  ["="],
  (lhs, rhs, span) => ({ kind: "assign", lhs, rhs, span }),
);

function parseExprUnary(t: State): [State, Expr<Parsed>] {
  const peek = peekKind(t);
  if (peek && UNARY_KINDS.includes(peek as UnaryKind)) {
    let tok: Token;
    [t, tok] = expectNext(t, peek);
    let rhs;
    [t, rhs] = parseExprUnary(t);
    return [
      t,
      {
        kind: "unary",
        unaryKind: tok.kind as UnaryKind,
        rhs,
        span: tok.span,
      },
    ];
  }

  return parseExprCall(t);
}
function parseExprCall(t: State): [State, Expr<Parsed>] {
  let lhs: Expr<Parsed>;
  [t, lhs] = parseExprAtom(t);

  while (peekKind(t) === "(" || peekKind(t) === ".") {
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
        span: lhs.span.merge(access.span),
      };
    }
  }

  return [t, lhs];
}

function parseExprAtom(startT: State): [State, Expr<Parsed>] {
  // eslint-disable-next-line prefer-const
  let [t, tok] = next(startT);
  const span = tok.span;

  if (tok.kind === "(") {
    let expr: Expr<Parsed>;
    [t, expr] = parseExpr(t);

    // This could be a block or a tuple literal. We can only know after
    // parsing the first expression and looking at the delimiter.

    const [, peek] = next(t);
    // It's a single element, which we interpret as a block.
    // `(0,)` is the one elem tuple.
    if (peek.kind === ")") {
      [t] = expectNext(t, ")");
      return [t, { kind: "block", span, exprs: [expr] }];
    }
    // It's a block.
    if (peek.kind === ";") {
      const exprs = [expr];
      while (peekKind(t) !== ")") {
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

    const nameIdent: Ident = { name: name.ident, span: name.span };

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

function parseStructInit(
  t: State,
): [State, ExprStructLiteral<Parsed>["fields"]] {
  [t] = expectNext(t, "{");

  let fields;
  [t, fields] = parseCommaSeparatedList<StructLiteralField<Parsed>>(
    t,
    "}",
    (t) => {
      let name;
      [t, name] = expectNext<TokenIdent>(t, "identifier");
      [t] = expectNext(t, ":");
      let expr;
      [t, expr] = parseExpr(t);

      return [t, { name: { name: name.ident, span: name.span }, expr }];
    },
  );

  return [t, fields];
}

function parseType(t: State): [State, Type<Parsed>] {
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
      if (peekKind(t) === ")") {
        [t] = next(t);
        return [t, { kind: "tuple", elems: [], span }];
      }
      let head;
      [t, head] = parseType(t);

      if (peekKind(t) === ")") {
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
        span,
      );
    }
  }
}

// helpers

function parseCommaSeparatedList<R>(
  t: State,
  terminator: Token["kind"],
  parser: Parser<R>,
): [State, R[]] {
  const items: R[] = [];

  // () | (a) | (a,) | (a, b)

  while (peekKind(t) !== terminator) {
    let nextValue;
    [t, nextValue] = parser(t);

    items.push(nextValue);

    let comma;
    [t, comma] = eat(t, ",");
    if (!comma) {
      // No comma? Fine, you don't like trailing commas.
      // But this better be the end.
      if (peekKind(t) !== terminator) {
        unexpectedToken(next(t)[1], `, or ${terminator}`);
      }
      break;
    }
  }

  [t] = expectNext(t, terminator);

  return [t, items];
}

function eat<T extends BaseToken>(
  t: State,
  kind: T["kind"],
): [State, T | undefined] {
  if (peekKind(t) === kind) {
    return expectNext(t, kind);
  }
  return [t, undefined];
}

function peekKind(t: State): Token["kind"] | undefined {
  return maybeNextT(t)?.[1]?.kind;
}

function expectNext<T extends BaseToken>(
  t: State,
  kind: T["kind"],
): [State, T & Token] {
  let tok;
  [t, tok] = maybeNextT(t);
  if (!tok) {
    throw new CompilerError(
      `expected \`${kind}\`, found end of file`,
      Span.eof(t.file),
    );
  }
  if (tok.kind !== kind) {
    throw new CompilerError(
      `expected \`${kind}\`, found \`${tok.kind}\``,
      tok.span,
    );
  }
  return [t, tok as unknown as T & Token];
}

function next(t: State): [State, Token] {
  const [rest, next] = maybeNextT(t);
  if (!next) {
    throw new CompilerError("unexpected end of file", Span.eof(t.file));
  }
  return [rest, next];
}

function maybeNextT(t: State): [State, Token | undefined] {
  const next = t.tokens[0];
  const rest = t.tokens.slice(1);

  return [{ ...t, tokens: rest }, next];
}

function unexpectedToken(token: Token, expected: string): never {
  throw new CompilerError(`unexpected token, expected ${expected}`, token.span);
}

function validateAst(ast: Crate<Built>) {
  const seenItemIds = new ComplexSet();

  const validator: Folder<Built, Built> = {
    ...mkDefaultFolder(),
    itemInner(item: Item<Built>): Item<Built> {
      if (seenItemIds.has(item.id)) {
        throw new Error(
          `duplicate item id: ${item.id.toString()} for ${item.node.name}`,
        );
      }
      seenItemIds.add(item.id);
      return superFoldItem(item, this);
    },
    expr(expr: Expr<Built>): Expr<Built> {
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
        const checkPrecedence = (inner: Expr<Built>, side: string) => {
          if (inner.kind === "binary") {
            const ourClass = binaryExprPrecedenceClass(expr.binaryKind);
            const innerClass = binaryExprPrecedenceClass(inner.binaryKind);

            if (ourClass !== innerClass) {
              throw new CompilerError(
                `mixing operators without parentheses is not allowed. ${side} is ${inner.binaryKind}, which is different from ${expr.binaryKind}`,
                expr.span,
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
    ident(ident) {
      return ident;
    },
    type(type) {
      return type;
    },
  };

  foldAst(ast, validator);
}

function buildCrate(
  packageName: string,
  rootItems: Item<Parsed>[],
  crateId: number,
  rootFile: LoadedFile,
): Crate<Built> {
  const itemId = new Ids();
  itemId.next(); // crate root ID
  const loopId = new Ids();

  const ast: Crate<Built> = {
    id: crateId,
    rootItems,
    itemsById: new ComplexMap(),
    packageName,
    rootFile,
  };

  const assigner: Folder<Parsed, Built> = {
    ...mkDefaultFolder(),
    itemInner(item: Item<Parsed>): Item<Built> {
      const id = new ItemId(crateId, itemId.next());
      return { ...superFoldItem(item, this), id };
    },
    expr(expr: Expr<Parsed>): Expr<Built> {
      if (expr.kind === "loop") {
        return {
          ...(superFoldExpr(expr, this) as ExprLoop<Built> & Expr<Built>),
          loopId: loopId.next(),
        };
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

  const crate = foldAst(ast, assigner);

  return crate;
}
