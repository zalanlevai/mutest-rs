use std::path::PathBuf;

use mutest_inspector::config::{self, Config};
use mutest_inspector::ctxt::TargetSpec;

#[tokio::main]
async fn main() {
    let matches = clap::command!()
        .propagate_version(true)
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
        .arg(clap::arg!(--"metadata-root-dir" [JSON_ROOT_DIR] "Path to JSON metadata files from a mutest-rs analysis.").value_parser(clap::value_parser!(PathBuf)).default_value("target/mutest/json"))
        .arg(clap::arg!(--port [PORT] "Port number to serve interface on.").value_parser(clap::value_parser!(u16)).default_value("3000"))
        .arg(clap::arg!(--open [TARGET_SPEC] "Open the interface in a browser. Optionally, a specific package to open may be specified by name, or a target by target spec syntax (e.g., `package/lib`, or `package/test:integration_test`).").num_args(0..=1).require_equals(true))
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand.").action(clap::ArgAction::Help).global(true))
        .arg(clap::arg!(-V --version "Print version information.").action(clap::ArgAction::Version).global(true))
        .get_matches();

    let Some(metadata_root_dir_path) = matches.get_one::<PathBuf>("metadata-root-dir").cloned() else { unreachable!() };

    let port = *matches.get_one::<u16>("port").unwrap();

    let open = match matches.value_source("open") {
        Some(clap::parser::ValueSource::CommandLine) => {
            match matches.get_one::<String>("open") {
                Some(target_str) => {
                    match target_str.split_once("/") {
                        None => {
                            let package = target_str.to_owned();
                            Some(config::OpenTarget::Specific(package, None))
                        }
                        Some((package, target_spec_str)) => 'specific_target_arm: {
                            let package = package.to_owned();
                            let Ok(target) = TargetSpec::from_path_str(target_spec_str) else {
                                color_print::ceprintln!("<yellow, bold>warning</>: invalid target selector `{}` specified", target_spec_str);
                                break 'specific_target_arm Some(config::OpenTarget::Specific(package, None));
                            };
                            Some(config::OpenTarget::Specific(package, Some(target)))
                        }
                    }
                }
                None => Some(config::OpenTarget::Workspace),
            }
        }
        _ => None,
    };

    let config = Config {
        metadata_root_dir_path,
        opts: config::Options {
            port,
            open,
        },
    };

    mutest_inspector::run(config).await;
}
