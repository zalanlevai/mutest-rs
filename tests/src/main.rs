#![feature(if_let_guard)]
#![feature(iter_collect_into)]
#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(round_char_boundary)]

use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::hash::{Hash, Hasher};
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
    StdOut { empty: bool },
    /// //@ stderr
    StdErr { empty: bool },
}

impl Expectation {
    pub fn display_name(&self) -> &str {
        match self {
            Expectation::StdOut { .. } => "stdout",
            Expectation::StdErr { .. } => "stderr",
        }
    }

    pub fn check(&self, path: &Path, stdout: &str, stderr: &str) -> ExpectationVerdict {
        match self {
            &Expectation::StdOut { empty: expect_empty } | &Expectation::StdErr { empty: expect_empty } => {
                let (out_name, out, out_path) = match self {
                    Expectation::StdOut { .. } => ("stdout", stdout, path.with_extension("stdout")),
                    Expectation::StdErr { .. } => ("stderr", stderr, path.with_extension("stderr")),
                    #[expect(unreachable_patterns)]
                    _ => unreachable!(),
                };

                if expect_empty {
                    if !out.is_empty() {
                        let diff_text = diff::display_diff("", out).unwrap();
                        return ExpectationVerdict::Unmet {
                            reason: format!("{out_name} is not empty"),
                            error: Some(diff_text),
                        };
                    }
                } else {
                    if !out_path.exists() { return ExpectationVerdict::Unblessed; }

                    let expected_out = fs::read_to_string(&out_path).expect(&format!("cannot read {}", out_path.display()));
                    if *out != expected_out {
                        let diff_text = diff::display_diff(&expected_out, out).unwrap();
                        return ExpectationVerdict::Unmet {
                            reason: format!("{out_name} does not match expected output"),
                            error: Some(diff_text),
                        };
                    }
                }


                ExpectationVerdict::Met
            }
        }
    }

    pub fn bless(&self, path: &Path, stdout: &str, stderr: &str, dry_run: bool) -> BlessVerdict {
        match self {
            &Expectation::StdOut { empty: expect_empty } | &Expectation::StdErr { empty: expect_empty } => {
                if expect_empty { return BlessVerdict::UpToDate; }

                let (_out_name, out, out_path) = match self {
                    Expectation::StdOut { .. } => ("stdout", stdout, path.with_extension("stdout")),
                    Expectation::StdErr { .. } => ("stderr", stderr, path.with_extension("stderr")),
                    #[expect(unreachable_patterns)]
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

const BUILD_OUT_DIR: &str = "target/mutest_test/debug/deps";
const AUX_OUT_DIR: &str = "target/mutest_test/debug/deps/auxiliary";

struct Opts {
    pub filters: Option<Vec<String>>,
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

enum TestResult {
    Ignored,
    Failed,
    Ok,
    New,
    Blessed,
}

fn log_test(test_name: &str, result: TestResult, reason: Option<&str>) {
    eprintln!("test {test_name} ... {result}{reason}",
        result = match result {
            TestResult::Ignored => "\x1b[1;33mignored\x1b[0m",
            TestResult::Failed => "\x1b[1;31mFAILED\x1b[0m",
            TestResult::Ok => "\x1b[1;32mok\x1b[0m",
            TestResult::New => "\x1b[1;33mNEW\x1b[0m",
            TestResult::Blessed => "\x1b[1;35mBLESSED\x1b[0m",
        },
        reason = reason.map(|s| format!(" ({s})")).unwrap_or("".to_owned()),
    );
}

fn parse_directives(path: &Path) -> Vec<String> {
    let source = fs::File::open(path).expect(&format!("cannot open `{}`", path.display()));
    let mut reader = BufReader::with_capacity(1024, source);

    let mut directives = vec![];
    let mut line = String::new();
    while reader.read_line(&mut line).expect(&format!("cannot read contents of `{}`", path.display())) >= 1 {
        if let Some(directive) = line.trim_start().strip_prefix("//@").map(str::trim) {
            directives.push(directive.to_owned());
        }
        line.clear();
    }

    directives
}

fn run_test(path: &Path, aux_dir_path: &Path, root_dir: &Path, opts: &Opts, results: &mut TestRunResults) {
    if !path.is_file() { return; }
    if !path.extension().is_some_and(|v| v == "rs") { return; }

    results.total_tests_count += 1;

    let display_path = path.strip_prefix(root_dir).expect("cannot strip root dir prefix").with_extension("");

    let name = display_path.to_string_lossy().into_owned();

    if let Some(filters) = &opts.filters {
        if !filters.iter().any(|filter| name.contains(filter)) { return; }
    }

    let unmangled_crate_name = display_path.components()
        .filter_map(|component| {
            match component {
                path::Component::Normal(component) => Some(component.to_str().expect("invalid path component")),
                _ => None,
            }
        })
        .intersperse("_")
        .collect::<String>();

    let crate_hash = {
        let mut hasher = std::hash::DefaultHasher::new();
        unmangled_crate_name.hash(&mut hasher);
        let hash = hasher.finish();
        format!("{hash:016x}")
    };

    let test_crate_name = format!("mutest_test_{crate_hash}_{crate_name}",
        crate_name = &unmangled_crate_name[..unmangled_crate_name.floor_char_boundary(48)],
    );

    let directives = parse_directives(&path);

    if directives.iter().any(|d| d == "ignore") {
        results.ignored_tests_count += 1;
        log_test(&name, TestResult::Ignored, None);
        return;
    }

    let mut edition: Option<&str> = None;
    let mut expect_command_fail = false;
    let mut expectations = BTreeSet::new();
    let mut mutest_prints = BTreeSet::new();
    let mut mutest_subcommand: Option<&str> = None;
    for directive in &directives {
        match directive.as_str() {
            subcommand @ ("print-tests" | "print-call-graph" | "print-targets" | "print-mutants" | "print-code" | "build" | "run") => {
                if let Some(previous_subcommand) = mutest_subcommand && previous_subcommand != "print" {
                    results.ignored_tests_count += 1;
                    log_test(&name, TestResult::Ignored, Some("invalid directives"));
                    return;
                }
                match subcommand {
                    "run" => mutest_subcommand = Some("build"),
                    "print-tests" => {
                        mutest_prints.insert("tests");
                        mutest_subcommand.get_or_insert("print");
                    }
                    "print-call-graph" => {
                        mutest_prints.insert("call-graph");
                        mutest_subcommand.get_or_insert("print");
                    }
                    "print-targets" => {
                        mutest_prints.insert("targets");
                        mutest_subcommand.get_or_insert("print");
                    }
                    "print-mutants" => {
                        mutest_prints.insert("mutants");
                        mutest_subcommand.get_or_insert("print");
                    }
                    "print-code" => {
                        mutest_prints.insert("code");
                        mutest_subcommand.get_or_insert("print");
                    }
                    subcommand => mutest_subcommand = Some(subcommand),
                };
            }

            "fail" => {
                if let Some(_previous_subcommand) = mutest_subcommand {
                    results.ignored_tests_count += 1;
                    log_test(&name, TestResult::Ignored, Some("invalid directives"));
                    return;
                }
                expect_command_fail = true;
                mutest_subcommand = Some("build");
            }

            _ if let Some(edition_str) = directive.strip_prefix("edition:").map(str::trim) => {
                if let Some(_previous_edition) = edition {
                    results.ignored_tests_count += 1;
                    log_test(&name, TestResult::Ignored, Some("invalid directives: multiple editions"));
                    return;
                }
                edition = Some(edition_str);
            }

            "stdout" => { expectations.insert(Expectation::StdOut { empty: false }); }
            "stdout: empty" => { expectations.insert(Expectation::StdOut { empty: true }); }
            "stderr" => { expectations.insert(Expectation::StdErr { empty: false }); }
            "stderr: empty" => { expectations.insert(Expectation::StdErr { empty: true }); }

            _ if directive.starts_with("aux-build:") => {}
            _ if directive.starts_with("rustc-flags:") => {}
            _ if directive.starts_with("verify:") => {}
            _ if directive.starts_with("mutation-operators:") => {}
            _ if directive.starts_with("mutest-flags:") => {}
            _ if directive.starts_with("mutest-subcommand-flags:") => {}

            _ => {
                results.ignored_tests_count += 1;
                log_test(&name, TestResult::Ignored, Some(&format!("unknown directive: `{directive}`")));
                return;
            }
        }
    }

    // Set defaults.
    let edition = edition.unwrap_or("2018");
    let mutest_subcommand = mutest_subcommand.unwrap_or("build");

    // HACK: We invoke mutest-driver directly, rather than through Cargo, which means
    //       we have to add `windows.lib` to the linker search path manually.
    //       See https://github.com/rust-lang/rust/issues/99466.
    #[cfg(windows)]
    let windows_lib = {
        // FIXME: Include all available `windows-targets` platform variants.
        #[cfg(all(target_arch = "x86_64", target_env = "msvc"))]
        const WINDOWS_IMPORT_LIB: &str = "windows_x86_64_msvc";
        #[cfg(all(target_arch = "aarch64", target_env = "msvc"))]
        const WINDOWS_IMPORT_LIB: &str = "windows_aarch64_msvc";

        let metadata_cmd = cargo_metadata::MetadataCommand::new();
        let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");
        let windows_package = metadata.packages.iter().find(|package| package.name == WINDOWS_IMPORT_LIB).expect("could not retrieve windows.lib import package from manifest");
        windows_package.manifest_path.parent().expect("invalid windows.lib import package manifest path").join("lib")
    };

    let mut aux = false;
    for directive in &directives {
        let Some(aux_build) = directive.strip_prefix("aux-build:").map(str::trim) else { continue; };
        aux = true;

        let aux_path = aux_dir_path.join(aux_build);
        let aux_crate_name = Path::new(aux_build).file_stem().expect("invalid aux path").to_str().expect("invalid aux path");

        let aux_directives = parse_directives(&aux_path);

        let mut edition = None;
        for aux_directive in &aux_directives {
            match aux_directive.as_str() {
                _ if let Some(edition_str) = directive.strip_prefix("edition:").map(str::trim) => {
                    if let Some(_previous_edition) = edition {
                        results.ignored_tests_count += 1;
                        log_test(&name, TestResult::Ignored, Some("invalid directives: multiple editions"));
                        return;
                    }
                    edition = Some(edition_str);
                }

                _ => {
                    results.ignored_tests_count += 1;
                    log_test(&name, TestResult::Ignored, Some(&format!("unknown directive: `{directive}`")));
                    return;
                }
            }
        }

        // Set defaults.
        let edition = edition.unwrap_or("2018");

        let mut cmd = Command::new("target/release/mutest-driver");
        // We need to invoke mutest-driver as a rustc wrapper. This must be the first argument.
        cmd.arg("/dummy/rustc");

        cmd.arg(&aux_path);
        cmd.args(["--crate-name", aux_crate_name]);
        cmd.arg(format!("--edition={edition}"));

        cmd.args(["--out-dir", AUX_OUT_DIR]);

        #[cfg(windows)]
        cmd.args(["-L", windows_lib.as_str()]);

        cmd.args(["-L", AUX_OUT_DIR]);

        if opts.verbosity >= 1 {
            eprintln!("running {cmd:?}");
        }

        let output = cmd.output().expect("cannot spawn mutest-driver in rustc mode");
        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        if opts.verbosity >= 1 {
            if let Some(exit_code) = output.status.code() {
                eprintln!("exited with code {exit_code}");
            }
            eprintln!("stdout:\n{}", stdout);
            eprintln!("stderr:\n{}", stderr);
        }

        if output.status.code() != Some(0) {
            results.failed_tests_count += 1;
            log_test(&name, TestResult::Failed, Some(&match output.status.code() {
                Some(exit_code) => format!("process exited with code {exit_code}"),
                None => "process exited without exit code".to_owned(),
            }));
            eprintln!("stdout:\n{}", stdout);
            eprintln!("stderr:\n{}", stderr);
            return;
        }
    }

    let mut cmd = Command::new("target/release/mutest-driver");
    // We need to invoke mutest-driver as a rustc wrapper. This must be the first argument.
    cmd.arg("/dummy/rustc");

    cmd.arg(&path);
    cmd.args(["--crate-name".to_owned(), test_crate_name]);
    cmd.arg(format!("--edition={edition}"));

    cmd.args(["--crate-type", "lib"]);

    cmd.args(["--out-dir", BUILD_OUT_DIR]);

    // Trick mutest-driver into invoking its behaviour, rather than falling back to a rustc invocation.
    cmd.env("CARGO_PRIMARY_PACKAGE", "1");
    cmd.arg("--test");

    cmd.env("MUTEST_SEARCH_PATH", "target/release");

    #[cfg(windows)]
    cmd.args(["-L", windows_lib.as_str()]);

    let rustc_flags = directives.iter().filter_map(|d| d.strip_prefix("rustc-flags:").map(str::trim))
        .flat_map(|flags| flags.split(" ").filter(|flag| !flag.is_empty()));
    cmd.args(rustc_flags);

    if aux {
        cmd.args(["-L", AUX_OUT_DIR]);
    }

    let mut mutest_args = vec![];
    let mut verifications = directives.iter().filter_map(|d| d.strip_prefix("verify:").map(str::trim))
        .flat_map(|flags| flags.split(",").map(str::trim).filter(|flag| !flag.is_empty()))
        .peekable();
    if verifications.peek().is_some() {
        mutest_args.push("--Zverify".to_owned());
        mutest_args.push(verifications.intersperse(",").collect::<String>());
    }
    let mut mutation_operators = directives.iter().filter_map(|d| d.strip_prefix("mutation-operators:").map(str::trim))
        .flat_map(|flags| flags.split(",").map(str::trim).filter(|flag| !flag.is_empty()))
        .peekable();
    if mutation_operators.peek().is_some() {
        mutest_args.push("--mutation-operators".to_owned());
        mutest_args.push(mutation_operators.intersperse(",").collect::<String>());
    }
    if !mutest_prints.is_empty() {
        mutest_args.push("--print".to_owned());
        mutest_args.push(mutest_prints.into_iter().intersperse(",").collect::<String>());
    }
    directives.iter().filter_map(|d| d.strip_prefix("mutest-flags:").map(str::trim))
        .flat_map(|flags| flags.split(" ").filter(|flag| !flag.is_empty()).map(str::to_owned))
        .collect_into(&mut mutest_args);
    mutest_args.push(mutest_subcommand.to_owned());
    directives.iter().filter_map(|d| d.strip_prefix("mutest-subcommand-flags:").map(str::trim))
        .flat_map(|flags| flags.split(" ").filter(|flag| !flag.is_empty()).map(str::to_owned))
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

    let expected_exit_code = match (expect_command_fail, mutest_subcommand) {
        (true, _) => 1,
        (_, "run") => 101,
        (false, _) => 0,
    };

    if output.status.code() != Some(expected_exit_code) {
        results.failed_tests_count += 1;
        log_test(&name, TestResult::Failed, Some(&match output.status.code() {
            Some(exit_code) => format!("process exited with code {exit_code}, expected {expected_exit_code}"),
            None => format!("process exited without exit code, expected {expected_exit_code}"),
        }));
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
            log_test(&name, TestResult::Ok, None);
            return;
        }

        results.blessed_tests_count += 1;
        if bless_verdicts.iter().all(|v| matches!(v, BlessVerdict::New)) {
            results.new_tests_count += 1;
        }
        log_test(&name, TestResult::Blessed, None);

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
            log_test(&name, TestResult::Ok, None);
            return;
        }

        let has_unblessed_expectations = expectation_verdicts.iter().any(|v| matches!(v, ExpectationVerdict::Unblessed));

        if has_unblessed_expectations { results.new_tests_count += 1; }

        let unmet_expectation_verdicts = expectation_verdicts.iter()
            .filter(|v| matches!(v, ExpectationVerdict::Unmet { .. }))
            .collect::<Vec<_>>();

        match (&unmet_expectation_verdicts[..], has_unblessed_expectations) {
            ([], true) => {
                log_test(&name, TestResult::New, None);
            }
            ([], false) => unreachable!(),
            ([ExpectationVerdict::Unmet { reason, error }], _) => {
                results.failed_tests_count += 1;
                log_test(&name, TestResult::Failed, Some(reason));
                if let Some(error) = error {
                    eprintln!("{error}");
                }
            }
            (unmet_expectation_verdicts, _) => {
                results.failed_tests_count += 1;
                log_test(&name, TestResult::Failed, Some(&format!("{} expectations failed", unmet_expectation_verdicts.len())));
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
        .arg(clap::arg!(--filter [FILTER] "Only run tests matching any of one or more filter(s)."))
        .arg(clap::arg!(-v --verbose "Print more verbose information during execution.").action(clap::ArgAction::Count).default_value("0").display_order(100))
        .arg(clap::arg!(-h --help "Print help information; this message.").action(clap::ArgAction::Help).display_order(999).global(true))
        .get_matches();

    let bless = matches.get_flag("bless");
    let dry_run = matches.get_flag("dry-run");
    let verbosity = matches.get_count("verbose");

    let filters = matches.get_one::<String>("filter").map(|s| s.split(",").map(|f| f.trim().to_owned()).collect::<Vec<_>>());

    let opts = Opts {
        filters,
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

    // Ensure build output directory exists.
    fs::create_dir_all(BUILD_OUT_DIR).expect("cannot create build output directory");

    fn run_tests_in_dir(dir_path: &Path, opts: &Opts, results: &mut TestRunResults) {
        fn run_tests_in_dir_impl(root_dir: &Path, dir_path: &Path, opts: &Opts, results: &mut TestRunResults) {
            let aux_dir_path = dir_path.join("auxiliary");

            for entry in fs::read_dir(dir_path).expect(&format!("cannot read `{}` directory", dir_path.display())) {
                let entry = entry.expect(&format!("cannot read entry in `{}` directory", dir_path.display()));
                let path = entry.path();

                #[cfg(windows)]
                let path = {
                    use std::path::PathBuf;
                    use path_slash::PathBufExt;
                    PathBuf::from(path.to_slash_lossy().to_string())
                };

                if path.is_dir() {
                    if path.file_name().is_some_and(|v| v == "auxiliary") { continue; };
                    run_tests_in_dir_impl(root_dir, &path, opts, results);
                    continue;
                }

                if !path.is_file() { continue; }
                if !path.extension().is_some_and(|v| v == "rs") { continue; }

                run_test(&path, &aux_dir_path, root_dir, opts, results);
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
