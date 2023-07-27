import { CompilerError, Span } from "./error";

export type DatalessToken =
  | "function"
  | "let"
  | "in"
  | "if"
  | "then"
  | "else"
  | "type"
  | "("
  | ")"
  | "["
  | "]"
  | ";"
  | ":"
  | ","
  | "="
  | "+"
  | "-"
  | "*"
  | "/"
  | "&"
  | "|"
  | "!"
  | "<"
  | ">"
  | "=="
  | "<="
  | ">="
  | "!="
  | "!";

export type TokenIdent = { kind: "identifier"; ident: string };

export type TokenLit =
  | {
      kind: "lit_string";
      value: string;
    }
  | {
      kind: "lit_int";
      value: number;
    };

export type TokenKind = { kind: DatalessToken } | TokenIdent | TokenLit;

export type Token = TokenKind & {
  span: Span;
};

export type BaseToken = { kind: Token["kind"] };

const SINGLE_PUNCT: string[] = [
  "(",
  ")",
  "[",
  "]",
  ";",
  ":",
  ",",
  "+",
  "-",
  "*",
  "/",
  "&",
  "|",
];

export function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  finish: while (i < input.length) {
    const next = input[i];
    const span: Span = { start: i, end: i + 1 };

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
          while (true) {
            const next = input[i + 1];
            span.end++;
            i++;
            if (next === '"') {
              break;
            }
            if (next === undefined) {
              throw new CompilerError(`Unterminated string literal`, span);
            }
          }
          const value = input.slice(span.start + 1, span.end - 1);
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
                `\`${digit}\` was tokenized to a number even though it is not`
              );
            }

            tokens.push({ kind: "lit_int", value: int, span });
          } else if (isIdentStart(next)) {
            while (isIdentContinue(input[i + 1])) {
              span.end++;
              i++;
            }
            const ident = input.slice(span.start, span.end);
            let kw = isKeyword(ident);
            if (kw) {
              tokens.push({ kind: kw, span });
            } else {
              tokens.push({ kind: "identifier", span, ident: ident });
            }
          } else if (isWhitespace(next)) {
            // ignore
          } else {
            throw new CompilerError(`Invalid character: \`${next}\``, span);
          }
        }
      }
    }

    i++;
  }

  return tokens;
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

const keywords = new Set<string>([
  "function",
  "let",
  "in",
  "if",
  "then",
  "else",
  "type",
]);
function isKeyword(kw: string): DatalessToken | undefined {
  return keywords.has(kw) ? (kw as DatalessToken) : undefined;
}
