import { CompilerError, Span } from "./error";

export type DatalessToken =
  | "kw_function"
  | "kw_let"
  | "p_popen"
  | "p_pclose"
  | "p_bopen"
  | "p_bclose"
  | "p_semi";

export type TokenKind =
  | { kind: DatalessToken }
  | { kind: "identifier"; ident: string }
  | { kind: "lit_string"; value: string };

export type Token = TokenKind & {
  span: Span;
};

export function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  finish: while (i < input.length) {
    const next = input[i];
    const span: Span = { start: i, end: i + 1 };
    switch (next) {
      case undefined: {
        break finish;
      }
      case "(": {
        tokens.push({ kind: "p_popen", span });
        break;
      }
      case ")": {
        tokens.push({ kind: "p_pclose", span });
        break;
      }
      case "{": {
        tokens.push({ kind: "p_bopen", span });
        break;
      }
      case "}": {
        tokens.push({ kind: "p_bclose", span });
        break;
      }
      case ";": {
        tokens.push({ kind: "p_semi", span });
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
          throw new Error("digit");
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

const keywords = new Map<string, DatalessToken>([
  ["function", "kw_function"],
  ["let", "kw_let"],
]);
function isKeyword(kw: string): DatalessToken | undefined {
  return keywords.get(kw);
}
