#![feature(iter_collect_into)]
#![feature(iter_intersperse)]

use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::iter;
use std::path::{self, Path};
use std::process::{self, Command, Stdio};
use std::str;
use std::time::Instant;

mod diff;

#[derive(Debug)]
enum ExpectationVerdict {
    Met,
    Unblessed,
    Unmet { reason: String, error: Option<String> },
}

#[derive(Debug)]
enum BlessVerdict {
    New,
    Changed(String),
    UpToDate,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Expectation {
    /// //@ stdout
    StdOut,
    /// //@ stderr
    StdErr,
}

impl Expectation {
    pub fn display_name(&self) -> &str {
        match self {
            Expectation::StdOut => "stdout",
            Expectation::StdErr => "stderr",
        }
    }

    pub fn check(&self, path: &Path, stdout: &str, stderr: &str) -> ExpectationVerdict {
        match self {
            Expectation::StdOut | Expectation::StdErr => {
                let (out_name, out, out_path) = match self {
                    Expectation::StdOut => ("stdout", stdout, path.with_extension("stdout")),
                    Expectation::StdErr => ("stderr", stderr, path.with_extension("stderr")),
                    _ => unreachable!(),
                };

                if !out_path.exists() { return ExpectationVerdict::Unblessed; }

                let expected_out = fs::read_to_string(&out_path).expect(&format!("cannot read {}", out_path.display()));
                if *out != expected_out {
                    let diff_text = diff::display_diff(&expected_out, out).unwrap();
                    return ExpectationVerdict::Unmet {
                        reason: format!("{out_name} does not match expected output"),
                        error: Some(diff_text),
                    };
                }

                ExpectationVerdict::Met
            }
        }
    }

    pub fn bless(&self, path: &Path, stdout: &str, stderr: &str, dry_run: bool) -> BlessVerdict {
        match self {
            Expectation::StdOut | Expectation::StdErr => {
                let (out_name, out, out_path) = match self {
                    Expectation::StdOut => ("stdout", stdout, path.with_extension("stdout")),
                    Expectation::StdErr => ("stderr", stderr, path.with_extension("stderr")),
                    _ => unreachable!(),
                };

                let previous_out = out_path.exists().then(|| fs::read_to_string(&out_path).expect(&format!("cannot read {}", out_path.display())));

                if previous_out.as_deref() != Some(out) {
                    if !dry_run {
                        fs::write(&out_path, out).expect(&format!("cannot write {}", out_path.display()));
                    }

                    return match previous_out {
                        Some(previous_out) => {
                            let diff_text = diff::display_diff(&previous_out, out).unwrap();
                            BlessVerdict::Changed(diff_text)
                        }
                        None => BlessVerdict::New
                    }
                }

                BlessVerdict::UpToDate
            }
        }
    }
}

struct Opts {
    pub bless: bool,
    pub dry_run: bool,
    pub verbosity: u8,
}

struct TestRunResults {
    pub ignored_tests_count: usize,
    pub passed_tests_count: usize,
    pub failed_tests_count: usize,
    pub new_tests_count: usize,
    pub blessed_tests_count: usize,
    pub total_tests_count: usize,
}

fn run_test(path: &Path, root_dir: &Path, opts: &Opts, results: &mut TestRunResults) {
    if !path.is_file() { return; }
    if !path.extension().is_some_and(|v| v == "rs") { return; }

    results.total_tests_count += 1;

    let display_path = path.strip_prefix(root_dir).expect("cannot strip root dir prefix").with_extension("");

    let name = display_path.to_string_lossy();

    let test_crate_name = format!("mutest_test_{crate_name}",
        crate_name = display_path.components()
            .filter_map(|component| {
                match component {
                    path::Component::Normal(component) => Some(component.to_str().expect("invalid path component")),
                    _ => None,
                }
            })
            .intersperse("_")
            .collect::<String>(),
    );

    let source = fs::File::open(&path).expect(&format!("cannot open `{}`", path.display()));
    let mut reader = BufReader::with_capacity(1024, source);

    let mut directives = vec![];
    let mut line = String::new();
    while reader.read_line(&mut line).expect(&format!("cannot read contents of `{}`", path.display())) >= 1 {
        let Some(directive) = line.trim_start().strip_prefix("//@").map(str::trim) else { continue; };

        directives.push(directive.to_owned());

        line.clear();
    }

    if directives.iter().any(|d| d == "ignore") {
        results.ignored_tests_count += 1;
        eprintln!("test {name} ... \x1b[1;33mignored\x1b[0m");
        return;
    }

    let mut expectations = BTreeSet::new();
    let mutest_subcommand = {
        let mut mutest_subcommand = None;

        for directive in &directives {
            match directive.as_str() {
                subcommand @ ("print-targets" | "print-mutants" | "print-code" | "build" | "run") => {
                    if let Some(_previous_subcommand) = mutest_subcommand {
                        results.ignored_tests_count += 1;
                        eprintln!("test {name} ... \x1b[1;33mignored\x1b[0m (invalid directives)");
                        return;
                    }
                    mutest_subcommand = Some(subcommand);
                }

                "stdout" => { expectations.insert(Expectation::StdOut); }
                "stderr" => { expectations.insert(Expectation::StdErr); }

                _ if directive.starts_with("mutest-flags:") => {}
                _ if directive.starts_with("mutest-subcommand-flags:") => {}

                _ => {
                    results.ignored_tests_count += 1;
                    eprintln!("test {name} ... \x1b[1;33mignored\x1b[0m (unknown directive: `{directive}`)");
                    return;
                }
            }
        }

        mutest_subcommand.unwrap_or("build")
    };

    let mut cmd = Command::new("target/release/mutest-driver");
    // We need to invoke mutest-driver as a rustc wrapper. This must be the first argument.
    cmd.arg("/dummy/rustc");

    cmd.arg(&path);
    cmd.args(["--crate-name".to_owned(), test_crate_name]);
    cmd.arg("--edition=2021");

    cmd.args(["--crate-type", "lib"]);

    cmd.args(["--out-dir", "target/mutest_test/debug/deps"]);

    // Trick mutest-driver into invoking its behaviour, rather than falling back to a rustc invocation.
    cmd.env("CARGO_PRIMARY_PACKAGE", "1");
    cmd.arg("--test");

    cmd.env("MUTEST_SEARCH_PATH", "target/release");

    let mut mutest_args = vec![];
    directives.iter().filter_map(|d| d.strip_prefix("mutest-flags:").map(str::trim))
        .flat_map(|flags| flags.split(" ").filter(|flag| !flag.is_empty()))
        .collect_into(&mut mutest_args);
    mutest_args.push(mutest_subcommand);
    directives.iter().filter_map(|d| d.strip_prefix("mutest-subcommand-flags:").map(str::trim))
        .flat_map(|flags| flags.split(" ").filter(|flag| !flag.is_empty()))
        .collect_into(&mut mutest_args);
    cmd.env("MUTEST_ARGS".to_owned(), mutest_args.join(" "));

    if opts.verbosity >= 1 {
        eprintln!("running {cmd:?}");
    }

    let output = cmd.output().expect("cannot spawn mutest-driver");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    if opts.verbosity >= 1 {
        if let Some(exit_code) = output.status.code() {
            eprintln!("exited with code {exit_code}");
        }
        eprintln!("stdout:\n{}", stdout);
        eprintln!("stderr:\n{}", stderr);
    }

    let expected_exit_code = match mutest_subcommand {
        "run" => 101,
        _ => 0,
    };

    // TODO: Some tests may expect to fail
    if output.status.code() != Some(expected_exit_code) {
        results.failed_tests_count += 1;
        eprintln!("test {name} ... \x1b[1;31mFAILED\x1b[0m ({reason})",
            reason = match output.status.code() {
                Some(exit_code) => format!("process exited with code {exit_code}"),
                None => "process exited without exit code".to_owned(),
            },
        );
        eprintln!("stdout:\n{}", stdout);
        eprintln!("stderr:\n{}", stderr);
        return;
    }

    if opts.bless {
        let bless_verdicts = expectations.iter()
            .map(|expectation| expectation.bless(&path, &stdout, &stderr, opts.dry_run))
            .collect::<Vec<_>>();

        if bless_verdicts.iter().all(|v| matches!(v, BlessVerdict::UpToDate)) {
            results.passed_tests_count += 1;
            eprintln!("test {name} ... \x1b[1;32mok\x1b[0m");
            return;
        }

        results.blessed_tests_count += 1;
        if bless_verdicts.iter().all(|v| matches!(v, BlessVerdict::New)) {
            results.new_tests_count += 1;
        }
        eprintln!("test {name} ... \x1b[1;35mBLESSED\x1b[0m");

        for (expectation, bless_verdict) in iter::zip(&expectations, &bless_verdicts) {
            match bless_verdict {
                BlessVerdict::New => {}
                BlessVerdict::Changed(change) => {
                    eprintln!("{}:\n{change}", expectation.display_name());
                }
                BlessVerdict::UpToDate => {}
            }
        }
    } else {
        let expectation_verdicts = expectations.iter()
            .map(|expectation| expectation.check(&path, &stdout, &stderr))
            .collect::<Vec<_>>();

        if expectation_verdicts.iter().all(|v| matches!(v, ExpectationVerdict::Met)) {
            results.passed_tests_count += 1;
            eprintln!("test {name} ... \x1b[1;32mok\x1b[0m");
            return;
        }

        let has_unblessed_expectations = expectation_verdicts.iter().any(|v| matches!(v, ExpectationVerdict::Unblessed));

        if has_unblessed_expectations { results.new_tests_count += 1; }

        let unmet_expectation_verdicts = expectation_verdicts.iter()
            .filter(|v| matches!(v, ExpectationVerdict::Unmet { .. }))
            .collect::<Vec<_>>();

        match (&unmet_expectation_verdicts[..], has_unblessed_expectations) {
            ([], true) => {
                eprintln!("test {name} ... \x1b[1;33mNEW\x1b[0m");
            }
            ([], false) => unreachable!(),
            ([ExpectationVerdict::Unmet { reason, error }], _) => {
                results.failed_tests_count += 1;
                eprintln!("test {name} ... \x1b[1;31mFAILED\x1b[0m ({reason})");
                if let Some(error) = error {
                    eprintln!("{error}");
                }
            }
            (unmet_expectation_verdicts, _) => {
                results.failed_tests_count += 1;
                eprintln!("test {name} ... \x1b[1;31mFAILED\x1b[0m ({} expectations failed)", unmet_expectation_verdicts.len());
                for unmet_expectation_verdict in unmet_expectation_verdicts {
                    let ExpectationVerdict::Unmet { reason, error } = unmet_expectation_verdict else { unreachable!(); };
                    eprintln!("{reason}:");
                    if let Some(error) = error {
                        eprintln!("{error}");
                    }
                }
            }
        }
    }
}

fn main() {
    let matches = clap::command!()
        .bin_name("cargo run -p mutest-tests --")
        .disable_help_flag(true)
        .disable_version_flag(true)
        .arg(clap::arg!(--bless "Update expectation snapshots for new and existing tests."))
        .arg(clap::arg!(--"dry-run" "Do not modify the file system when blessing expectations."))
        .arg(clap::arg!(-v --verbose "Print more verbose information during execution.").action(clap::ArgAction::Count).default_value("0").display_order(100))
        .arg(clap::arg!(-h --help "Print help information; this message.").action(clap::ArgAction::Help).display_order(999).global(true))
        .get_matches();

    let bless = matches.get_flag("bless");
    let dry_run = matches.get_flag("dry-run");
    let verbosity = matches.get_count("verbose");

    let opts = Opts {
        bless,
        dry_run,
        verbosity,
    };

    // Ensure we are testing latest mutest-driver.
    let mut cmd = Command::new("cargo");
    cmd.args(["build", "--release", "-p", "mutest-driver"]);
    cmd.stdout(Stdio::inherit());
    cmd.stderr(Stdio::inherit());
    if !cmd.output().expect("cannot spawn cargo").status.success() {
        eprintln!("`cargo build --release -p mutest-driver` failed");
        process::exit(1);
    }
    eprintln!();

    let mut results = TestRunResults {
        ignored_tests_count: 0,
        passed_tests_count: 0,
        failed_tests_count: 0,
        new_tests_count: 0,
        blessed_tests_count: 0,
        total_tests_count: 0,
    };

    let t_tests_start = Instant::now();

    fn run_tests_in_dir(dir_path: &Path, opts: &Opts, results: &mut TestRunResults) {
        fn run_tests_in_dir_impl(root_dir: &Path, dir_path: &Path, opts: &Opts, results: &mut TestRunResults) {
            for entry in fs::read_dir(dir_path).expect(&format!("cannot read `{}` directory", dir_path.display())) {
                let entry = entry.expect(&format!("cannot read entry in `{}` directory", dir_path.display()));
                let path = entry.path();

                if path.is_dir() {
                    run_tests_in_dir_impl(root_dir, &path, opts, results);
                    continue;
                }

                if !path.is_file() { continue; }
                if !path.extension().is_some_and(|v| v == "rs") { continue; }

                run_test(&path, root_dir, opts, results);
            }
        }

        run_tests_in_dir_impl(dir_path, dir_path, opts, results);
    }

    run_tests_in_dir(Path::new("tests/ui"), &opts, &mut results);

    let tests_duration = t_tests_start.elapsed();

    eprintln!();
    if opts.bless {
        eprintln!("test result: {result}. {blessed} blessed ({new} new); {passed} passed; {failed} failed; {ignored} ignored; finished in {duration:.2?}",
            result = match results.blessed_tests_count {
                0 => "\x1b[1;32mok\x1b[0m",
                _ => "\x1b[1;33mCHANGED\x1b[0m",
            },
            blessed = results.blessed_tests_count,
            new = results.new_tests_count,
            passed = results.passed_tests_count,
            failed = results.failed_tests_count,
            ignored = results.ignored_tests_count,
            duration = tests_duration,
        );
    } else {
        eprintln!("test result: {result}. {passed} passed; {failed} failed; {ignored} ignored; finished in {duration:.2?}",
            result = match results.failed_tests_count {
                0 => "\x1b[1;32mok\x1b[0m",
                _ => "\x1b[1;31mFAILED\x1b[0m",
            },
            passed = results.passed_tests_count,
            failed = results.failed_tests_count,
            ignored = results.ignored_tests_count,
            duration = tests_duration,
        );

        if results.new_tests_count >= 1 {
            eprintln!("note: encountered {new} tests with missing expectation snapshots, rerun with `--bless`",
                new = results.new_tests_count,
            );
        }
    }

    if results.failed_tests_count >= 1 {
        process::exit(101);
    }
}
