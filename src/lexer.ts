import {
  CompilerError,
  ErrorEmitted,
  ErrorHandler,
  LoadedFile,
  Span,
} from "./error";

export type DatalessToken =
  | "function"
  | "let"
  | "if"
  | "then"
  | "else"
  | "type"
  | "loop"
  | "break"
  | "import"
  | "extern"
  | "mod"
  | "global"
  | "struct"
  | "use"
  | "("
  | ")"
  | "{"
  | "}"
  | "["
  | "]"
  | ";"
  | ":"
  | "."
  | ","
  | "="
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "&"
  | "|"
  | "!"
  | "<"
  | ">"
  | "=="
  | "<="
  | ">="
  | "!="
  | "end of file";

export type TokenIdent = { kind: "identifier"; ident: string };

export type TokenLitString = {
  kind: "lit_string";
  value: string;
};

export type LitIntType = "Int" | "I32";

export type TokenLit =
  | TokenLitString
  | {
      kind: "lit_int";
      value: number;
      type: LitIntType;
    };

export type TokenKind = { kind: DatalessToken } | TokenIdent | TokenLit;

export type Token = TokenKind & {
  span: Span;
};

export type BaseToken = { kind: Token["kind"] };

const SINGLE_PUNCT: string[] = [
  "(",
  ")",
  "}",
  "{",
  "[",
  "]",
  ";",
  ":",
  ".",
  ",",
  "+",
  "-",
  "*",
  "/",
  "&",
  "|",
  "%",
];

class LexerError extends Error {
  constructor(public inner: CompilerError) {
    super("lexer error");
  }
}

export type LexerResult =
  | {
      ok: true;
      tokens: Token[];
    }
  | {
      ok: false;

      err: ErrorEmitted;
    };

export function tokenize(handler: ErrorHandler, file: LoadedFile): LexerResult {
  try {
    return { ok: true, tokens: tokenizeInner(file) };
  } catch (e) {
    if (e instanceof LexerError) {
      const err: ErrorEmitted = handler.emitError(e.inner);
      return { ok: false, err };
    } else {
      throw e;
    }
  }
}

function tokenizeInner(file: LoadedFile): Token[] {
  const err = (msg: string, span: Span) =>
    new LexerError(new CompilerError(msg, span));

  const { content: input } = file;
  const tokens: Token[] = [];
  let i = 0;

  finish: while (i < input.length) {
    const next = input[i];
    const span: Span = new Span(i, i + 1, file);

    if (next === "/" && input[i + 1] === "/") {
      while (input[i] !== "\n") {
        i++;
      }

      continue;
    }

    if (next === "/" && input[i + 1] === "*") {
      i++;
      i++;
      while (input[i] !== "*" && input[i + 1] !== "/") {
        i++;
        if (input[i] === undefined) {
          throw err("unterminated block comment", span);
        }
      }
      i++;
      i++;
      continue;
    }

    if (SINGLE_PUNCT.includes(next)) {
      tokens.push({ kind: next as DatalessToken, span });
    } else {
      switch (next) {
        case undefined: {
          break finish;
        }
        case "=": {
          if (input[i + 1] === "=") {
            span.end++;
            i++;
            tokens.push({ kind: "==", span });
          } else {
            tokens.push({ kind: "=", span });
          }
          break;
        }
        case ">": {
          if (input[i + 1] === "=") {
            span.end++;
            i++;
            tokens.push({ kind: ">=", span });
          } else {
            tokens.push({ kind: ">", span });
          }
          break;
        }
        case "<": {
          if (input[i + 1] === "=") {
            span.end++;
            i++;
            tokens.push({ kind: "<=", span });
          } else {
            tokens.push({ kind: "<", span });
          }
          break;
        }
        case "!": {
          if (input[i + 1] === "=") {
            span.end++;
            i++;
            tokens.push({ kind: "!=", span });
          } else {
            tokens.push({ kind: "!", span });
          }
          break;
        }
        case '"': {
          const result = [];
          while (true) {
            const next = input[i + 1];
            span.end++;
            i++;
            if (next === '"') {
              break;
            }

            if (next === "\\") {
              span.end++;
              i++;
              switch (input[i]) {
                case "\\":
                  result.push("\\");
                  break;
                case '"':
                  result.push('"');
                  break;
                case "n":
                  result.push("\n");
                  break;
                case "r":
                  result.push("\r");
                  break;
                case "t":
                  result.push("\t");
                  break;
                case "a":
                  result.push("\x07");
                  break;
                case "3":
                  // device control 3 for callie's big project
                  result.push("\x13");
                  break;
                case "M":
                  // end of medium for callie's big project
                  result.push("\x19");
                  break;
                default:
                  throw err(
                    `invalid escape character: ${input[i]}`,
                    new Span(span.end - 1, span.end, file),
                  );
              }
              continue;
            }

            result.push(next);
            if (next === undefined) {
              throw err(`Unterminated string literal`, span);
            }
          }
          const value = result.join("");
          tokens.push({ kind: "lit_string", span, value });
          break;
        }
        default: {
          if (isDigit(next)) {
            while (isDigit(input[i + 1])) {
              span.end++;
              i++;
            }
            const digit = input.slice(span.start, span.end);
            const int = parseInt(digit, 10);
            if (Number.isNaN(int)) {
              throw new Error(
                `\`${digit}\` was tokenized to a number even though it is not`,
              );
            }

            let type: LitIntType = "Int";
            if (input[i + 1] === "_" && isIdentStart(input[i + 2])) {
              if (input.slice(i + 2, i + 5) === "Int") {
                i += 4;
                type = "Int";
              } else if (input.slice(i + 2, i + 5) === "I32") {
                i += 4;
                type = "I32";
              }
            }

            tokens.push({ kind: "lit_int", value: int, span, type });
          } else if (isIdentStart(next)) {
            while (isIdentContinue(input[i + 1])) {
              span.end++;
              i++;
            }
            const ident = input.slice(span.start, span.end);
            const kw = isKeyword(ident);
            if (kw) {
              tokens.push({ kind: kw, span });
            } else {
              tokens.push({ kind: "identifier", span, ident: ident });
            }
          } else if (isWhitespace(next)) {
            // ignore
          } else {
            throw err(`invalid character: \`${next}\``, span);
          }
        }
      }
    }

    i++;
  }

  return tokens;
}

export function isValidIdent(ident: string): boolean {
  if (!isIdentStart(ident[0])) {
    return false;
  }
  for (let i = 1; i < ident.length; i++) {
    const char = ident[i];
    if (!isIdentContinue(char)) {
      return false;
    }
  }
  return true;
}

function isIdentStart(char: string): boolean {
  return (
    (char <= "Z" && char >= "A") || (char <= "z" && char >= "a") || char === "_"
  );
}

function isIdentContinue(char: string): boolean {
  return (
    (char <= "Z" && char >= "A") ||
    (char <= "z" && char >= "a") ||
    char === "_" ||
    isDigit(char)
  );
}

function isDigit(char: string): boolean {
  return !Number.isNaN(parseInt(char, 10));
}

function isWhitespace(char: string): boolean {
  return char === " " || char === "\t" || char === "\n" || char === "\r";
}

const KEYOWRDS: DatalessToken[] = [
  "function",
  "let",
  "if",
  "then",
  "else",
  "type",
  "loop",
  "break",
  "import",
  "extern",
  "mod",
  "global",
  "struct",
  "use",
];

const KEYWORD_SET = new Set<string>(KEYOWRDS);
function isKeyword(kw: string): DatalessToken | undefined {
  return KEYWORD_SET.has(kw) ? (kw as DatalessToken) : undefined;
}
