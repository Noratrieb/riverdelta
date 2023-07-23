const input = `
function ok() {}
`;

function main() {
  const tokens = tokenize(input);
  console.log(tokens);
}

type Span = {
  start: number;
  len: number;
};

type DatalessToken =
  | "kw_function"
  | "p_popen"
  | "p_pclose"
  | "p_bopen"
  | "p_bclose";

type TokenKind =
  | { kind: DatalessToken }
  | { kind: "identifier"; ident: string };

type Token = TokenKind & {
  span: Span;
};

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  finish: while (i < input.length) {
    const next = input[i];
    const span: Span = { start: i, len: 1 };
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
      default: {
        if (isDigit(next)) {
          throw new Error("digit");
        } else if (isIdentStart(next)) {
          while (isIdentContinue(input[i + 1])) {
            span.len++;
            i++;
          }
          const ident = input.slice(span.start, span.start + span.len);
          tokens.push({ kind: "identifier", span, ident: ident });
        } else if (isWhitespace(next)) {
          // ignore
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

main();
