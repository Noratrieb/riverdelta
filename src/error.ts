export type Span = {
  start: number;
  end: number;
};

export function spanMerge(a: Span, b: Span): Span {
  return {
    start: Math.min(a.start, b.start),
    end: Math.max(a.end, b.end),
  };
}

export const DUMMY_SPAN = { start: 0, end: 0 };

export class CompilerError extends Error {
  msg: string;
  span: Span;

  constructor(msg: string, span: Span) {
    super(msg);
    this.msg = msg;
    this.span = span;
  }
}

export function withErrorHandler(input: string, f: () => void) {
  try {
    f();
  } catch (e) {
    if (e instanceof CompilerError) {
      renderError(input, e);
    } else {
      throw e;
    }
  }
}

function renderError(input: string, e: CompilerError) {
  const lineSpans = lines(input);
  const line =
    e.span.start === Number.MAX_SAFE_INTEGER
      ? lineSpans[lineSpans.length - 1]
      : lineSpans.find(
          (line) => line.start <= e.span.start && line.end >= e.span.start
        );
  if (!line) {
    throw Error(`Span out of bounds: ${e.span.start}..${e.span.end}`);
  }
  const lineIdx = lineSpans.indexOf(line);
  console.error(`error: ${e.message}`);

  console.error(`${lineIdx} | ${spanToSnippet(input, line)}`);
  const startRelLine =
    e.span.start === Number.MAX_SAFE_INTEGER ? 0 : e.span.start - line.start;
  console.error(
    `${" ".repeat(String(lineIdx).length)}   ${" ".repeat(startRelLine)}^`
  );
}

function spanToSnippet(input: string, span: Span): string {
  if (span.start === Number.MAX_SAFE_INTEGER) {
    return "";
  }
  return input.slice(span.start, span.end);
}

export function lines(input: string): Span[] {
  const lines: Span[] = [{ start: 0, end: 0 }];

  for (let i = 0; i < input.length; i++) {
    if (input[i] === "\n") {
      lines.push({ start: i + 1, end: i + 1 });
    } else {
      lines[lines.length - 1].end++;
    }
  }

  return lines;
}

export function todo(msg: string): never {
  throw new CompilerError(`TODO: ${msg}`, { start: 0, end: 0 });
}
