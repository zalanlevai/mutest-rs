use std::process;
use std::path::PathBuf;

use mutest_driver::config::{self, Config};

fn main() {
    let matches = clap::command!()
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        // Subcommands
        .subcommand(clap::Command::new("print-targets")
            .display_order(3)
            .about("Print list of functions targeted for mutation at the specified depth.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        .subcommand(clap::Command::new("print-code")
            .display_order(2)
            .about("Print the generated code of the test harness.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        .subcommand(clap::Command::new("build")
            .display_order(1)
            .about("Build the test harness.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        .subcommand(clap::Command::new("run")
            .display_order(0)
            .about("Build and run the test harness.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
            // Passed arguments
            .arg(clap::Arg::new("PASSED_ARGS").takes_value(true).multiple_values(true).allow_hyphen_values(true))
        )
        // Arguments
        .arg(clap::arg!(<PACKAGE_PATH> "Path to directory of package to analyze."))
        .arg(clap::arg!(--"crate-path" [CRATE_PATH] "Path to root of crate to analyze.").default_value("src/main.rs"))
        .arg(clap::arg!(-d --depth [DEPTH] "Callees of each test function are mutated up to the specified depth.").default_value("3").validator(str::parse::<usize>))
        .arg(clap::arg!(--"mutant-batch-size" [MUTANT_BATCH_SIZE] "Maximum number of mutations to batch into a single mutant.").default_value("1").validator(str::parse::<usize>))
        // Information
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand."))
        .arg(clap::arg!(-V --version "Print version information."))
        .subcommand(clap::Command::new("help").about("Print help information; this message or the help of the given subcommand."))
        .get_matches();

    let package_directory_path = matches.value_of("PACKAGE_PATH").map(PathBuf::from).unwrap();
    let crate_root_path = package_directory_path.join(matches.value_of("crate-path").map(PathBuf::from).unwrap());

    let mode = match matches.subcommand() {
        Some(("print-targets", _)) => config::Mode::PrintMutationTargets,
        Some(("print-code", _)) => config::Mode::PrintCode,
        Some(("build", _)) => config::Mode::Build,
        Some(("run", matches)) => {
            let passed_args = matches.values_of("PASSED_ARGS").unwrap_or_default().map(ToOwned::to_owned).collect();
            config::Mode::BuildAndRun(passed_args)
        }
        _ => unreachable!(),
    };

    let mutation_depth = matches.value_of_t("depth").unwrap();
    let mutant_max_mutations_count = matches.value_of_t("mutant-batch-size").unwrap();

    let config = Config {
        package_directory_path,
        crate_root_path: Some(crate_root_path),
        opts: config::Options {
            mode,
            operators: &[
                &mutest_operators::ArgDefaultShadow,
                &mutest_operators::BitOpOrAndSwap,
                &mutest_operators::BitOpOrXorSwap,
                &mutest_operators::BitOpShiftDirSwap,
                &mutest_operators::BitOpXorAndSwap,
                &mutest_operators::ContinueBreakSwap,
                &mutest_operators::EqOpInvert,
                &mutest_operators::MathOpAddMulSwap,
                &mutest_operators::MathOpAddSubSwap,
                &mutest_operators::MathOpDivRemSwap,
                &mutest_operators::MathOpMulDivSwap,
                &mutest_operators::RangeLimitSwap,
                &mutest_operators::RelationalOpEqSwap,
                &mutest_operators::RelationalOpInvert,
            ],
            mutation_depth,
            mutant_max_mutations_count,
        },
    };

    match mutest_driver::main(config) {
        Ok(status) => process::exit(status),
        Err(_) => process::exit(-1),
    }
}
