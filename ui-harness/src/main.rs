use ui_test::{
    default_per_file_config, status_emitter, Args, CommandBuilder, Config, OutputConflictHandling,
};

fn main() -> ui_test::Result<()> {
    std::process::Command::new("cargo")
        .args(&[
            "build",
            "--manifest-path",
            "ui-harness/Cargo.toml",
            "--bin",
            "nilc-wrapper",
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .success()
        .then_some(())
        .unwrap_or_else(|| std::process::exit(1));

    let mut config = Config::rustc("./ui-tests");
    config.host = Some("wasm :3".into());
    config.program = CommandBuilder::cmd("ui-harness/target/debug/nilc-wrapper");

    let args = Args::parse_args(Args::default(), std::env::args().skip(1))?;

    config.with_args(&args);

    if !args.check && std::env::var_os("GITHUB_ACTIONS").is_none() {
        config.output_conflict_handling = OutputConflictHandling::Bless;
    }

    let result = ui_test::run_tests_generic(
        vec![config],
        |path, config| {
            if !path.extension().is_some_and(|ext| ext == "nil") {
                return None;
            }
            Some(ui_test::default_any_file_filter(path, config))
        },
        default_per_file_config,
        status_emitter::Text::quiet(),
    );
    if let Err(result) = result {
        println!("{:?}", result);
    }
    Ok(())
}
