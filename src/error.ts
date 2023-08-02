export type LoadedFile = {
  path?: string;
  content: string;
};

export type Span = {
  start: number;
  end: number;
  file: LoadedFile;
};

export function spanMerge(a: Span, b: Span): Span {
  if (a.file !== b.file) {
    throw new Error("cannot merge spans from different files");
  }

  return {
    start: Math.min(a.start, b.start),
    end: Math.max(a.end, b.end),
    file: a.file,
  };
}

export const DUMMY_SPAN: Span = { start: 0, end: 0, file: { content: "" } };
export const eofSpan = (file: LoadedFile): Span => ({
  start: Number.MAX_SAFE_INTEGER,
  end: Number.MAX_SAFE_INTEGER,
  file,
});

export class CompilerError extends Error {
  msg: string;
  span: Span;

  constructor(msg: string, span: Span) {
    super(msg);
    this.msg = msg;
    this.span = span;
  }
}

export function withErrorPrinter<R>(
  f: () => R,
  afterError: (e: CompilerError) => R
): R {
  try {
    return f();
  } catch (e) {
    if (e instanceof CompilerError) {
      renderError(e);
      return afterError(e);
    } else {
      throw e;
    }
  }
}

function renderError(e: CompilerError) {
  const { span } = e;
  const { content } = span.file;

  const lineSpans = lines(span.file);
  const line =
    span.start === Number.MAX_SAFE_INTEGER
      ? lineSpans[lineSpans.length - 1]
      : lineSpans.find(
          (line) => line.start <= span.start && line.end >= span.start
        );
  if (!line) {
    throw Error(`Span out of bounds: ${span.start}..${span.end}`);
  }
  const lineIdx = lineSpans.indexOf(line);
  const lineNo = lineIdx + 1;
  console.error(`error: ${e.message}`);
  console.error(` --> ${span.file.path ?? "<unknown>"}:${lineNo}`);

  console.error(`${lineNo} | ${spanToSnippet(content, line)}`);
  const startRelLine =
    span.start === Number.MAX_SAFE_INTEGER ? 0 : span.start - line.start;

  const spanLength =
    span.start === Number.MAX_SAFE_INTEGER
      ? 1
      : min(span.end, line.end) - span.start;

  console.error(
    `${" ".repeat(String(lineNo).length)}   ${" ".repeat(
      startRelLine
    )}${"^".repeat(spanLength)}`
  );
}

function spanToSnippet(input: string, span: Span): string {
  if (span.start === Number.MAX_SAFE_INTEGER) {
    return "";
  }
  return input.slice(span.start, span.end);
}

export function lines(file: LoadedFile): Span[] {
  const lines: Span[] = [{ start: 0, end: 0, file }];

  for (let i = 0; i < file.content.length; i++) {
    if (file.content[i] === "\n") {
      lines.push({ start: i + 1, end: i + 1, file });
    } else {
      lines[lines.length - 1].end++;
    }
  }

  return lines;
}

function min(a: number, b: number): number {
  return a < b ? a : b;
}
