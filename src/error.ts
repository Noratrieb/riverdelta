import chalk from "chalk";

export type LoadedFile = {
  path?: string;
  content: string;
};

export class Span {
  constructor(
    public start: number,
    public end: number,
    public file: LoadedFile,
  ) {}

  public merge(b: Span): Span {
    if (this.file !== b.file) {
      throw new Error("cannot merge spans from different files");
    }

    return new Span(
      Math.min(this.start, b.start),
      Math.max(this.end, b.end),
      this.file,
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

export type Emitter = (string: string) => void;

export class ErrorHandler {
  private errors: CompilerError[] = [];

  constructor(
    private emitter = (msg: string) => globalThis.console.error(msg),
  ) {}

  public emitError(err: CompilerError): ErrorEmitted {
    renderDiagnostic(this.emitter, err, (msg) => chalk.red(`error: ${msg}`));
    this.errors.push(err);
    return ERROR_EMITTED;
  }

  public warn(err: CompilerError): void {
    renderDiagnostic(this.emitter, err, (msg) =>
      chalk.yellow(`warning: ${msg}`),
    );
  }

  public hasErrors(): boolean {
    return this.errors.length > 0;
  }
}

const ERROR_EMITTED = Symbol();
export type ErrorEmitted = typeof ERROR_EMITTED;

export class CompilerError {
  msg: string;
  span: Span;

  constructor(msg: string, span: Span) {
    this.msg = msg;
    this.span = span;
  }
}

// Shadow console.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const console = {};

function renderDiagnostic(
  emitter: Emitter,
  e: CompilerError,
  render_msg: (msg: string) => string,
) {
  const { span } = e;
  const { content } = span.file;

  const lineSpans = lines(span.file);
  const line =
    span.start === Number.MAX_SAFE_INTEGER
      ? lineSpans[lineSpans.length - 1]
      : lineSpans.find(
          (line) => line.start <= span.start && line.end >= span.start,
        );
  if (!line) {
    throw Error(`Span out of bounds: ${span.start}..${span.end}`);
  }
  const lineIdx = lineSpans.indexOf(line);
  const lineNo = lineIdx + 1;
  emitter(render_msg(e.msg));
  emitter(` --> ${span.file.path ?? "<unknown>"}:${lineNo}`);

  emitter(`${lineNo} | ${spanToSnippet(content, line)}`);
  const startRelLine =
    span.start === Number.MAX_SAFE_INTEGER ? 0 : span.start - line.start;

  const spanLength =
    span.start === Number.MAX_SAFE_INTEGER
      ? 1
      : min(span.end, line.end) - span.start;

  emitter(
    `${" ".repeat(String(lineNo).length)}   ${" ".repeat(
      startRelLine,
    )}${"^".repeat(spanLength)}`,
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

export function unreachable(msg?: string): never {
  throw new Error(
    `entered unreachable code${msg !== undefined ? `: ${msg}` : ""}`,
  );
}
