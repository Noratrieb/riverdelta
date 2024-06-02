use std::io::Write;

use cargo_metadata::diagnostic::DiagnosticLevel;
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
struct RiverdeltaError {
    kind: cargo_metadata::diagnostic::DiagnosticLevel,
    message: String,
    span: RiverdeltaErrorSpan,
    rendered: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RiverdeltaErrorSpan {
    line_number: u64,
    file_name: String,
    byte_start: u32,
    byte_end: u32,
}

#[derive(Serialize)]
pub struct RustcDiagnostic {
    pub message: String,
    pub level: DiagnosticLevel,
    pub spans: Vec<RustcDiagnosticSpan>,
    pub children: Vec<()>,
    pub rendered: String,
}

#[derive(Serialize)]
pub struct RustcDiagnosticSpan {
    pub file_name: String,
    /// The byte offset in the file where this diagnostic starts from.
    pub byte_start: u32,
    /// The byte offset in the file where this diagnostic ends.
    pub byte_end: u32,
    /// 1-based. The line in the file.
    pub line_start: u64,
    /// 1-based. The line in the file.
    pub line_end: u64,
    /// 1-based, character offset.
    pub column_start: u64,
    /// 1-based, character offset.
    pub column_end: u64,
    /// Is this a "primary" span -- meaning the point, or one of the points,
    /// where the error occurred?
    ///
    /// There are rare cases where multiple spans are marked as primary,
    /// e.g. "immutable borrow occurs here" and "mutable borrow ends here" can
    /// be two separate spans both "primary". Top (parent) messages should
    /// always have at least one primary span, unless it has 0 spans. Child
    /// messages may have 0 or more primary spans.
    pub is_primary: bool,
    /// Source text from the start of line_start to the end of line_end.
    pub text: Vec<()>,
}

fn main() {
    let mut args = std::env::args().skip(1).collect::<Vec<_>>();

    let mut after_edition = false;
    args.retain(|arg| {
        // drop some stuff we dont care about
        if arg.starts_with("--crate-type") {
            return false;
        }
        if arg == "--edition" {
            after_edition = true;
            return false;
        }
        if after_edition {
            after_edition = false;
            return false;
        }

        true
    });

    // We don't want out.wat polluting things.
    args.push("--no-output".into());
    args.push("--error-format-json".into());

    let result = std::process::Command::new("node")
        .arg(".")
        .args(args)
        .output()
        .unwrap();

    std::io::stdout().write_all(&result.stdout).unwrap();

    let stderr = String::from_utf8(result.stderr).unwrap();
    for line in stderr.lines() {
        if line.starts_with("{") {
            let err: RiverdeltaError = serde_json::from_str::<RiverdeltaError>(line).unwrap();

            let rustc_error = RustcDiagnostic {
                message: err.message,
                level: err.kind,
                spans: vec![RustcDiagnosticSpan {
                    file_name: err.span.file_name,
                    byte_start: err.span.byte_start,
                    byte_end: err.span.byte_end,
                    line_start: err.span.line_number,
                    line_end: err.span.line_number,
                    column_start: 1,
                    column_end: 1,
                    is_primary: true,
                    text: vec![],
                }],
                children: vec![],
                rendered: err.rendered,
            };

            eprintln!("{}", serde_json::to_string(&rustc_error).unwrap());
        } else {
            eprintln!("{line}");
        }
    }

    let code = result.status.code().unwrap_or(1);
    std::process::exit(code);
}
