export type LoadedFile = {
  path?: string;
  content: string;
};

export class Span {
  constructor(
    public start: number,
    public end: number,
    public file: LoadedFile
  ) {}

  public merge(b: Span): Span {
    if (this.file !== b.file) {
      throw new Error("cannot merge spans from different files");
    }

    return new Span(
      Math.min(this.start, b.start),
      Math.max(this.end, b.end),
      this.file
    );
  }

  public static eof(file: LoadedFile): Span {
    return new Span(Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER, file);
  }

  public static startOfFile(file: LoadedFile): Span {
    return new Span(0, 1, file);
  }

  public static DUMMY: Span = new Span(0, 0, { content: "" });
}

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
  const lines: Span[] = [new Span(0, 0, file)];

  for (let i = 0; i < file.content.length; i++) {
    if (file.content[i] === "\n") {
      lines.push(new Span(i + 1, i + 1, file));
    } else {
      lines[lines.length - 1].end++;
    }
  }

  return lines;
}

function min(a: number, b: number): number {
  return a < b ? a : b;
}
