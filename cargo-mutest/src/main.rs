use std::env;
use std::process::{self, Command};

fn strip_arg(args: &mut Vec<String>, has_value: bool, short_arg: Option<&str>, long_arg: Option<&str>) {
    let short_arg = short_arg.map(|v| format!("-{v}"));
    let long_arg = long_arg.map(|v| format!("--{v}"));

    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];
        let arg_without_prefix = short_arg.as_deref().and_then(|v| arg.strip_prefix(v))
            .or_else(|| long_arg.as_deref().and_then(|v| arg.strip_prefix(v)));

        match arg_without_prefix.map(|v| !has_value || (v.trim_start().starts_with("=") && !(i + 1 < args.len()))) {
            Some(false) => { args.splice(i..=(i + 1), None); }
            Some(true) => { args.remove(i); }
            None => i += 1,
        }
    }
}

fn main() {
    let args = env::args().skip(2).collect::<Vec<_>>();

    let matches = mutest_driver::cli::command()
        .bin_name("cargo mutest")
        .no_binary_name(true)
        .about("Mutation testing tools for Rust")
        // Subcommands
        .subcommand(clap::Command::new("run")
            .display_order(0)
            .about("Build and run the test harness.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
            // Passed arguments
            .arg(clap::Arg::new("PASSED_ARGS").takes_value(true).multiple_values(true).allow_hyphen_values(true))
        )
        // Cargo
        .next_help_heading("CARGO OPTIONS")
        .arg(clap::arg!(--"manifest-path" [MANIFEST_PATH] "Path to Cargo.toml."))
        .arg(clap::arg!(--"target-dir" [TARGET_DIR] "Directory for all generated artifacts."))
        .arg(clap::arg!(--workspace "Test all packages in the workspace."))
        .arg(clap::arg!(-p --package [PACKAGE] "Package with the target to analyze."))
        .arg(clap::arg!(-F --features [FEATURES]... "Space or comma separated list of features to activate."))
        .arg(clap::arg!(--"all-features" "Activate all available features."))
        .arg(clap::arg!(--"no-default-features" "Do not activate the `default` feature."))
        .arg(clap::arg!(-r --release "Build artifacts in release mode, with optimizations."))
        .arg(clap::arg!(--profile [PROFILE] "Build artifacts with the specified profile."))
        .arg(clap::arg!(--lib "Test only this package's library unit tests."))
        .arg(clap::arg!(--bin [BINARY] "Test only the specified binary."))
        .arg(clap::arg!(--bins "Test all binaries."))
        .arg(clap::arg!(--"all-targets" "Test all targets."))
        .arg(clap::arg!(--offline "Run without accessing the network."))
        .get_matches_from(&args);

    let (cargo_subcommand, cargo_args, mutest_driver_subcommand, passed_args): (_, &[&str], _, _) = match matches.subcommand() {
        Some(("print-targets", _)) => ("check", &["--profile", "test"], "print-targets", None),
        Some(("print-mutants", _)) => ("check", &["--profile", "test"], "print-mutants", None),
        Some(("print-code", _)) => ("check", &["--profile", "test"], "print-code", None),
        Some(("build", _)) => ("test", &["--no-run"], "build", None),
        Some(("run", matches)) => {
            let passed_args = matches.values_of("PASSED_ARGS").unwrap_or_default().map(ToOwned::to_owned).collect::<Vec<_>>();
            ("test", &[], "build", Some(passed_args))
        }
        _ => unreachable!(),
    };

    let mut cmd = Command::new("cargo");
    cmd.arg(cargo_subcommand);
    cmd.args(cargo_args);

    let mut mutest_args = args.clone();
    let i = mutest_args.iter().position(|arg| matches.subcommand_name().is_some_and(|subcommand| arg == subcommand)).expect("subcommand not found in args");
    mutest_args.splice(i..(i + 1), [mutest_driver_subcommand.to_owned()]);

    let mut metadata_cmd = cargo_metadata::MetadataCommand::new();

    if let Some(manifest_path) = matches.value_of("manifest-path") {
        metadata_cmd.manifest_path(manifest_path);
        cmd.args(["--manifest-path", manifest_path]);
        strip_arg(&mut mutest_args, true, None, Some("manifest-path"));
    }
    if let Some(workspace) = matches.value_of("workspace") {
        cmd.args(["--workspace", workspace]);
        strip_arg(&mut mutest_args, true, None, Some("workspace"));
    }

    if let Some(package) = matches.value_of("package") {
        cmd.args(["--package", package]);
        strip_arg(&mut mutest_args, true, Some("p"), Some("package"));
    }

    if let Some(features) = matches.values_of("features") {
        metadata_cmd.features(cargo_metadata::CargoOpt::SomeFeatures(features.clone().map(ToOwned::to_owned).collect()));
        for feature in features { cmd.args(["--features", feature]); }
        strip_arg(&mut mutest_args, true, Some("F"), Some("features"));
    }
    if matches.is_present("all-features") {
        metadata_cmd.features(cargo_metadata::CargoOpt::AllFeatures);
        cmd.arg("--all-features");
        strip_arg(&mut mutest_args, false, None, Some("all-features"));
    }
    if matches.is_present("no-default-features") {
        metadata_cmd.features(cargo_metadata::CargoOpt::NoDefaultFeatures);
        cmd.arg("--no-default-features");
        strip_arg(&mut mutest_args, false, None, Some("no-default-features"));
    }

    let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");

    let target_dir = matches.value_of("target-dir").map(ToOwned::to_owned)
        .unwrap_or(metadata.target_directory.join("mutest").into_string());
    cmd.args(["--target-dir", &target_dir]);

    if matches.is_present("release") {
        cmd.arg("--release");
        strip_arg(&mut mutest_args, false, Some("r"), Some("release"));
    }
    if let Some(profile) = matches.value_of("profile") {
        cmd.args(["--profile", profile]);
        strip_arg(&mut mutest_args, true, None, Some("profile"));
    }

    if matches.is_present("lib") {
        cmd.arg("--lib");
        strip_arg(&mut mutest_args, false, None, Some("lib"));
    }
    if let Some(bin) = matches.value_of("bin") {
        cmd.args(["--bin", bin]);
        strip_arg(&mut mutest_args, true, None, Some("bin"));
    }
    if matches.is_present("bins") {
        cmd.arg("--bins");
        strip_arg(&mut mutest_args, false, None, Some("bins"));
    }
    if matches.is_present("all-targets") {
        cmd.arg("--all-targets");
        strip_arg(&mut mutest_args, false, None, Some("all-targets"));
    }

    if matches.is_present("offline") {
        cmd.arg("--offline");
        strip_arg(&mut mutest_args, false, None, Some("offline"));
    }

    let mut path = env::current_exe().expect("current executable path invalid");
    path.set_file_name("mutest-driver");
    if cfg!(windows) { path.set_extension("exe"); }
    cmd.env("RUSTC_WORKSPACE_WRAPPER", path);

    cmd.env("MUTEST_ARGS", mutest_args.join(" "));

    if let Some(passed_args) = passed_args {
        cmd.arg("--");
        if matches.is_present("timings") { cmd.arg("--timings"); }
        cmd.args(&passed_args);
    }

    let exit_status = cmd
        .spawn().expect("failed to run Cargo")
        .wait().expect("failed to run Cargo");

    process::exit(exit_status.code().unwrap_or(-1));
}
