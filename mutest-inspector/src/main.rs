use std::path::PathBuf;

use mutest_inspector::config::{self, Config};

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
        .arg(clap::arg!(--"json-dir" [JSON_DIR] "Path to JSON metadata files from a mutest-rs analysis.").value_parser(clap::value_parser!(PathBuf)).default_value("target/mutest/json"))
        .arg(clap::arg!(--port [PORT] "Port number to serve interface on.").value_parser(clap::value_parser!(u16)).default_value("3000"))
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand.").action(clap::ArgAction::Help).global(true))
        .arg(clap::arg!(-V --version "Print version information.").action(clap::ArgAction::Version).global(true))
        .get_matches();

    let Some(json_dir_path) = matches.get_one::<PathBuf>("json-dir").cloned() else { unreachable!() };

    let port = *matches.get_one::<u16>("port").unwrap();

    let config = Config {
        json_dir_path,
        opts: config::Options {
            port,
        },
    };

    mutest_inspector::run(config).await;
}
