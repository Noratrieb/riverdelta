use ui_test::{
    clap::Parser, default_filter_by_arg, default_per_file_config, status_emitter, Args,
    CommandBuilder, Config, Mode, OutputConflictHandling,
};

fn main() {
    std::process::Command::new("cargo")
        .args(&[
            "build",
            "--manifest-path",
            "tests/Cargo.toml",
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

    let mut config = Config::rustc("tests/ui");
    config.host = Some("wasm :3".into());
    config.program = CommandBuilder::cmd("tests/target/debug/nilc-wrapper");
    config.mode = Mode::Fail {
        require_patterns: false,
    };

    let args = Args::parse();

    let text = if args.quiet {
        status_emitter::Text::quiet()
    } else {
        status_emitter::Text::verbose()
    };

    if !args.check && std::env::var_os("GITHUB_ACTIONS").is_none() {
        config.output_conflict_handling = OutputConflictHandling::Bless;
    }

    let result = ui_test::run_tests_generic(
        config,
        args,
        |path, args| {
            path.extension().is_some_and(|ext| ext == "nil") && default_filter_by_arg(path, args)
        },
        default_per_file_config,
        (text, status_emitter::Gha::<true> { name: "ui".into() }),
    );
    if let Err(result) = result {
        println!("{:?}", result);
    }
}
