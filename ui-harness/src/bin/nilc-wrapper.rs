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

    let result = std::process::Command::new("node")
        .arg(".")
        .args(args)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let code = result.code().unwrap_or(1);
    std::process::exit(code);
}
