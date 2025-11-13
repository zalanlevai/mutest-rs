#![feature(if_let_guard)]

use std::collections::HashSet;
use std::env;
use std::path::PathBuf;

use mutest_runtime_embedded_host_driver::config::{self, Config};

mod print {
    mutest_driver_cli::opts! { ALL, pub(crate) possible_values where
        DETECTION_MATRIX = "detection-matrix"; ["Print test-mutation detection matrix."]
        SUBSUMPTION_MATRIX = "subsumption-matrix"; ["Print mutation subsumption matrix."]
    }
}

fn main() {
    let args = env::args().collect::<Vec<_>>();

    let [_, test_bin_arg, args @ ..] = &args[..] else {
        panic!("expected path to test binary as the first argument");
    };

    let test_bin_path = PathBuf::from(test_bin_arg);

    let matches = clap::command!()
        .no_binary_name(true)
        .disable_help_flag(true)
        .disable_version_flag(true)
        .styles({
            use clap::builder::styling::*;
            Styles::styled()
                .header(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightGreen))).bold())
                .usage(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightGreen))).bold())
                .literal(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightBlue))).bold())
                .placeholder(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightBlue))))
        })
        // Evaluation-related Arguments
        .arg(clap::arg!(--simulate [MUTATION_ID] "Evaluate tests for a single mutation.").value_parser(clap::value_parser!(u32)).conflicts_with_all(["flakes", "exhaustive", "print"]).display_order(110))
        .arg(clap::arg!(--flakes [ITERATIONS_COUNT] "Perform mutation analysis multiple times to find flaky test-mutation pairs.").value_parser(clap::value_parser!(usize)).display_order(111))
        .arg(clap::arg!(--exhaustive "Evaluate remaining tests, even if the mutation has already been detected by another test.").display_order(115))
        // Printing-related Arguments
        .arg(clap::arg!(--timings "Print timing information for each completed pass.").display_order(100))
        .arg(clap::arg!(-v --verbose "Print more verbose information during execution.").action(clap::ArgAction::Count).default_value("0").display_order(100))
        .arg(clap::arg!(--print [PRINT] "Print additional information during mutation evaluation. Multiple may be specified, separated by commas.").value_delimiter(',').action(clap::ArgAction::Append).value_parser(print::possible_values()).display_order(101))
        // Experimental Flags
        .arg(clap::arg!(--"Zwrite-json" [OUT_DIR] "Write JSON metadata files into the specified output directory.").value_parser(clap::value_parser!(PathBuf)).display_order(500))
        .arg(clap::arg!(--"Zwrite-json-eval-stream" "Write JSONL stream file into JSON output directory specified by `--Zwrite-json`.").display_order(500))
        // Information
        // FIXME: Regression; the `help` subcommand can no longer be customized,
        //        so the about text does not match that of the help flags.
        .arg(clap::arg!(-h --help "Print help information; this message.").action(clap::ArgAction::Help).global(true))
        .arg(clap::arg!(-V --version "Print version information.").action(clap::ArgAction::Version).global(true))
        .get_matches_from(args);

    let mode = match () {
        _ if let Some(&mutation_id) = matches.get_one::<u32>("simulate") => {
            config::Mode::Simulate { mutation_id }
        }
        _ if let Some(&iterations_count) = matches.get_one::<usize>("flakes") => {
            config::Mode::Flakes { iterations_count }
        }

        _ => config::Mode::Evaluate,
    };

    let exhaustive = matches.get_flag("exhaustive");

    let verbosity = matches.get_count("verbose");
    let report_timings = matches.get_flag("timings");

    let print_opts = {
        use crate::print as opts;

        let mut print_names = matches.get_many::<String>("print").map(|print| print.map(String::as_str).collect::<HashSet<_>>()).unwrap_or_default();
        if print_names.contains("all") { print_names = HashSet::from_iter(opts::ALL.into_iter().map(|s| *s)); }

        let mut print_opts = config::PrintOptions {
            detection_matrix: None,
            subsumption_matrix: None,
        };

        for print_name in print_names {
            match print_name {
                opts::DETECTION_MATRIX => print_opts.detection_matrix = Some(()),
                opts::SUBSUMPTION_MATRIX => print_opts.subsumption_matrix = Some(()),
                _ => unreachable!("invalid print information name: `{print_name}`"),
            }
        }

        print_opts
    };

    let write_opts = 'write_opts: {
        let Some(out_dir) = matches.get_one::<PathBuf>("Zwrite-json").cloned() else {
            break 'write_opts None;
        };
        // TODO: Determine specific package target out dir path from the test bin being tested.

        let eval_stream = matches.get_flag("Zwrite-json-eval-stream");

        Some(config::WriteOptions {
            out_dir,
            eval_stream: eval_stream.then_some(()),
        })
    };

    let config = Config {
        test_bin_path,

        opts: config::Options {
            mode,
            verbosity,
            report_timings,
            print_opts,
            write_opts,
            exhaustive,
        },
    };

    mutest_runtime_embedded_host_driver::run(config);
}
