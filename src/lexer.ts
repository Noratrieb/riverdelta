import { CompilerError, Span } from "./error";

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
  | "!";

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

export function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  finish: while (i < input.length) {
    const next = input[i];
    const span: Span = { start: i, end: i + 1 };

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
          throw new CompilerError("unterminated block comment", span);
        }
      }
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
                  throw new CompilerError(
                    `invalid escape character: ${input[i]}`,
                    { start: span.end - 1, end: span.end }
                  );
              }
              continue;
            }

            result.push(next);
            if (next === undefined) {
              throw new CompilerError(`Unterminated string literal`, span);
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
                `\`${digit}\` was tokenized to a number even though it is not`
              );
            }

            let type: LitIntType = "Int";
            console.log(input[i + 2]);
            if (input[i + 1] === "_" && isIdentStart(input[i + 2])) {
              console.log("yes", input.slice(i + 2, i + 5));

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
            throw new CompilerError(`invalid character: \`${next}\``, span);
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
];

const KEYWORD_SET = new Set<string>(KEYOWRDS);
function isKeyword(kw: string): DatalessToken | undefined {
  return KEYWORD_SET.has(kw) ? (kw as DatalessToken) : undefined;
}
