import chalk from "chalk";
import { Options } from "./options";

export type LoadedFile = {
  path: string;
  content: string;
};

export type ErrorFormat = "text-render" | "json";

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
    return new Span(file.content.length, file.content.length, file);
  }

  public static startOfFile(file: LoadedFile): Span {
    return new Span(0, 1, file);
  }

  public static DUMMY: Span = new Span(0, 0, { content: "", path: "" });
}

export type Emitter = (string: string) => void;

export class ErrorHandler {
  private errors: CompilerError[] = [];
  private opts: Options;

  constructor(
    opts: Options,
    private emitter = (msg: string) => globalThis.console.error(msg),
  ) {
    this.opts = opts;
  }

  public emitError(err: CompilerError): ErrorEmitted {
    if (this.opts.treatErrAsBug) {
      throw new Error(`--treat-err-as-bug: ${err.msg}`);
    }
    emitDiagnostic(this.opts, this.emitter, err, "error");
    this.errors.push(err);
    return ERROR_EMITTED;
  }

  public warn(err: CompilerError): void {
    emitDiagnostic(this.opts, this.emitter, err, "warning");
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

type JsonDiagnostic = {
  kind: "error" | "warning";
  message: string;
  span: {
    fileName: string;
    byteStart: number;
    byteEnd: number;
    lineNumber: number;
  };
  rendered: string;
};

function emitDiagnostic(
  opts: Options,
  emitter: Emitter,
  e: CompilerError,
  kind: "error" | "warning",
) {
  switch (opts.errorFormat) {
    case "json": {
      let rendered = "";
      renderDiagnostic((msg) => (rendered += `${msg}\n`), e, kind);

      const [lineNumber] = spanToLine(e.span);
      const object: JsonDiagnostic = {
        kind: kind,
        message: e.msg,
        span: {
          // TODO: this is *characters*, not bytes. lol.
          byteStart: e.span.start,
          byteEnd: e.span.start + e.span.end,
          fileName: e.span.file.path,
          lineNumber,
        },
        rendered,
      };
      emitter(JSON.stringify(object));

      break;
    }
    case "text-render": {
      renderDiagnostic(emitter, e, kind);
      break;
    }
  }
}

function renderDiagnostic(
  emitter: Emitter,
  e: CompilerError,
  kind: "error" | "warning",
) {
  const { span } = e;
  const { content } = span.file;

  const [lineNo, line] = spanToLine(span);
  if (kind === "error") {
    emitter(chalk.red(`error: ${e.msg}`));
  } else {
    emitter(chalk.yellow(`warning: ${e.msg}`));
  }
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

function spanToLine(span: Span): [number, Span] {
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
  return [lineNo, line];
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
