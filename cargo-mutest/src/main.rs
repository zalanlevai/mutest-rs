use std::collections::HashSet;
use std::env;
use std::path::PathBuf;
use std::process::{self, Command};

pub mod build {
    pub const RUST_TOOLCHAIN_VERSION: &str = env!("RUST_TOOLCHAIN_VERSION");
}

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

mod run_isolate {
    mutest_driver_cli::exclusive_opts! { pub(crate) possible_values where
        UNSAFE = "unsafe"; ["Only isolate tests for unsafe mutations."]
        ALL = "all"; ["Isolate tests for all mutations."]
    }
}

mod run_print {
    mutest_driver_cli::opts! { ALL, pub(crate) possible_values where
        DETECTION_MATRIX = "detection-matrix"; ["Print test-mutation detection matrix."]
        SUBSUMPTION_MATRIX = "subsumption-matrix"; ["Print mutation subsumption matrix."]
    }
}

fn main() {
    let args = env::args().skip(2).collect::<Vec<_>>();

    let matches = mutest_driver_cli::command()
        .bin_name("cargo mutest")
        .no_binary_name(true)
        .about("Mutation testing tools for Rust")
        // Subcommands
        .subcommand(clap::Command::new("run")
            .display_order(0)
            .about("Build and run the test harness.")
            // Evaluation-related Arguments
            .arg(clap::arg!(--simulate [MUTATION_ID] "Evaluate tests for a single mutation.").value_parser(clap::value_parser!(u32)).conflicts_with_all(["flakes", "exhaustive", "print"]).display_order(110))
            .arg(clap::arg!(--flakes [ITERATIONS_COUNT] "Perform mutation analysis multiple times to find flaky test-mutation pairs.").value_parser(clap::value_parser!(usize)).display_order(111))
            .arg(clap::arg!(--exhaustive "Evaluate remaining tests, even if the mutation has already been detected by another test.").display_order(115))
            .arg(clap::arg!(--isolate [ISOLATION_MODE] "Isolate tests of mutations into separate processes.").value_parser(run_isolate::possible_values()).default_value(run_isolate::UNSAFE).display_order(120))
            .arg(clap::arg!(--"use-thread-pool" "Evaluate tests in a fixed-size thread pool.").display_order(120))
            // Printing-related Arguments
            .arg(clap::arg!(--print [PRINT] "Print additional information during mutation evaluation. Multiple may be specified, separated by commas.").value_delimiter(',').value_parser(run_print::possible_values()).display_order(101))
            // Experimental Flags
            .arg(clap::arg!(--"Zwrite-json-eval-stream" "Write JSONL stream file(s) into JSON output directory.").display_order(500))
            // Passed arguments
            .arg(clap::Arg::new("PASSED_ARGS").trailing_var_arg(true).allow_hyphen_values(true))
        )
        // Cargo
        .next_help_heading("Cargo options")
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
        Some(("print", _)) => ("check", &["--profile", "test"], "print", None),
        Some(("build", _)) => ("test", &["--no-run"], "build", None),
        Some(("run", matches)) => {
            let mut passed_args = matches.get_many::<String>("PASSED_ARGS").unwrap_or_default().map(ToOwned::to_owned).collect::<Vec<_>>();

            if let Some(mutation_id) = matches.get_one::<u32>("simulate") { passed_args.push(format!("--simulate={mutation_id}")); }
            if let Some(iterations_count) = matches.get_one::<usize>("flakes") { passed_args.push(format!("--flakes={iterations_count}")); }

            if matches.get_flag("exhaustive") { passed_args.push("--exhaustive".to_owned()); }

            if let Some(isolation_mode) = matches.get_one::<String>("isolate") { passed_args.push(format!("--isolate={isolation_mode}")); }
            if matches.get_flag("use-thread-pool") { passed_args.push("--use-thread-pool".to_owned()); }

            let mut print_names = matches.get_many::<String>("print").map(|print| print.map(String::as_str).collect::<HashSet<_>>()).unwrap_or_default();
            if print_names.contains("all") { print_names = HashSet::from_iter(run_print::ALL.into_iter().map(|s| *s)); }
            for print_name in print_names { passed_args.push(format!("--print={print_name}")); }

            if matches.get_flag("Zwrite-json-eval-stream") { passed_args.push("--Zwrite-json-eval-stream".to_owned()); }

            ("test", &[], "build", Some(passed_args))
        }
        _ => unreachable!(),
    };

    #[cfg(not(windows))]
    let mut cmd = {
        let mut cmd = Command::new("cargo");
        cmd.arg(format!("+{}", build::RUST_TOOLCHAIN_VERSION));
        cmd
    };
    #[cfg(windows)]
    let mut cmd = {
        let mut cmd = Command::new("rustup");
        cmd.arg("run");
        cmd.arg(build::RUST_TOOLCHAIN_VERSION);
        cmd.arg("cargo");
        cmd
    };

    cmd.arg(cargo_subcommand);
    cmd.args(cargo_args);

    let mut mutest_args = args.clone();
    let i = mutest_args.iter().position(|arg| matches.subcommand_name().is_some_and(|subcommand| arg == subcommand)).expect("subcommand not found in args");
    mutest_args.splice(i.., [mutest_driver_subcommand.to_owned()]);

    let mut metadata_cmd = cargo_metadata::MetadataCommand::new();

    if let Some(manifest_path) = matches.get_one::<String>("manifest-path") {
        metadata_cmd.manifest_path(manifest_path);
        cmd.args(["--manifest-path", manifest_path]);
        strip_arg(&mut mutest_args, true, None, Some("manifest-path"));
    }
    if matches.get_flag("workspace") {
        cmd.arg("--workspace");
        strip_arg(&mut mutest_args, false, None, Some("workspace"));
    }

    if let Some(package) = matches.get_one::<String>("package") {
        cmd.args(["--package", package]);
        strip_arg(&mut mutest_args, true, Some("p"), Some("package"));
    }

    if let Some(features) = matches.get_many::<String>("features") {
        metadata_cmd.features(cargo_metadata::CargoOpt::SomeFeatures(features.clone().map(ToOwned::to_owned).collect()));
        for feature in features { cmd.args(["--features", feature]); }
        strip_arg(&mut mutest_args, true, Some("F"), Some("features"));
    }
    if matches.get_flag("all-features") {
        metadata_cmd.features(cargo_metadata::CargoOpt::AllFeatures);
        cmd.arg("--all-features");
        strip_arg(&mut mutest_args, false, None, Some("all-features"));
    }
    if matches.get_flag("no-default-features") {
        metadata_cmd.features(cargo_metadata::CargoOpt::NoDefaultFeatures);
        cmd.arg("--no-default-features");
        strip_arg(&mut mutest_args, false, None, Some("no-default-features"));
    }

    let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");

    let target_dir = matches.get_one::<String>("target-dir").map(ToOwned::to_owned)
        .unwrap_or(metadata.target_directory.join("mutest").into_string());
    cmd.args(["--target-dir", &target_dir]);

    if matches.get_flag("release") {
        cmd.arg("--release");
        strip_arg(&mut mutest_args, false, Some("r"), Some("release"));
    }
    if let Some(profile) = matches.get_one::<String>("profile") {
        cmd.args(["--profile", profile]);
        strip_arg(&mut mutest_args, true, None, Some("profile"));
    }

    if matches.get_flag("lib") {
        cmd.arg("--lib");
        strip_arg(&mut mutest_args, false, None, Some("lib"));
    }
    if let Some(bin) = matches.get_one::<String>("bin") {
        cmd.args(["--bin", bin]);
        strip_arg(&mut mutest_args, true, None, Some("bin"));
    }
    if matches.get_flag("bins") {
        cmd.arg("--bins");
        strip_arg(&mut mutest_args, false, None, Some("bins"));
    }
    if matches.get_flag("all-targets") {
        cmd.arg("--all-targets");
        strip_arg(&mut mutest_args, false, None, Some("all-targets"));
    }

    if matches.get_flag("offline") {
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
        cmd.args((0..matches.get_count("verbose")).map(|_| "-v"));
        if matches.get_flag("timings") { cmd.arg("--timings"); }
        if let Some(out_dir) = matches.get_one::<PathBuf>("Zwrite-json") {
            // NOTE: The out dir path passed to the generated test binary must be canonicalized,
            //       as it will likely be run under a different cwd.
            let out_dir = out_dir.canonicalize().expect("cannot canonicalize out dir path");
            let out_dir = out_dir.as_os_str().to_str().expect("non-UTF-8 path");
            cmd.arg(format!("--Zwrite-json={out_dir}"));
        }
        cmd.args(&passed_args);
    }

    let exit_status = cmd
        .spawn().expect("failed to run Cargo")
        .wait().expect("failed to run Cargo");

    process::exit(exit_status.code().unwrap_or(-1));
}
