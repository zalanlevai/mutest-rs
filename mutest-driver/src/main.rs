#![feature(rustc_private)]
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

use std::env;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use mutest_driver::config::{self, Config};
use mutest_emit::analysis::hir::Unsafety;
use mutest_emit::codegen::mutation::UnsafeTargeting;
use rustc_interface::Config as CompilerConfig;

struct DefaultCallbacks;
impl rustc_driver::Callbacks for DefaultCallbacks {}

/// This is different from `DefaultCallbacks` in that it will instruct Cargo to track the value of the `MUTEST_ARGS`
/// environment variable.
struct RustcCallbacks {
    mutest_args: Option<String>,
}

impl rustc_driver::Callbacks for RustcCallbacks {
    fn config(&mut self, config: &mut CompilerConfig) {
        let args = self.mutest_args.take();
        config.parse_sess_created = Some(Box::new(move |parse_sess| {
            mutest_driver::passes::track_invocation_fingerprint(parse_sess, &args);
        }));
    }
}

/// Fetch the sysroot, looking from most to least specific to this invocation:
/// * runtime environment
///   * `SYSROOT`
///   * `RUSTUP_HOME` and `RUSTUP_TOOLCHAIN`
///   * `MULTIRUST_HOME` and MULTIRUST_TOOLCHAIN
/// * sysroot from rustc in the path
/// * compile-time environment
///   * `SYSROOT`
///   * `RUSTUP_HOME` and `RUSTUP_TOOLCHAIN`
///   * `MULTIRUST_HOME` and `MULTIRUST_TOOLCHAIN`
fn fetch_sysroot() -> Option<PathBuf> {
    fn toolchain_path(home: Option<String>, toolchain: Option<String>) -> Option<PathBuf> {
        match (home, toolchain) {
            (Some(home), Some(toolchain)) => {
                let mut path = PathBuf::from(home);
                path.push("toolchains");
                path.push(toolchain);
                Some(path)
            }
            _ => None,
        }
    }

    env::var("SYSROOT").ok().map(PathBuf::from)
        .or_else(|| toolchain_path(env::var("RUSTUP_HOME").ok(), env::var("RUSTUP_TOOLCHAIN").ok()))
        .or_else(|| toolchain_path(env::var("MULTIRUST_HOME").ok(), env::var("MULTIRUST_TOOLCHAIN").ok()))
        .or_else(|| {
            Command::new("rustc")
                .args(&["--print", "sysroot"])
                .output().ok()
                .and_then(|out| String::from_utf8(out.stdout).ok())
                .map(|s| PathBuf::from(s.trim()))
        })
        .or_else(|| option_env!("SYSROOT").map(PathBuf::from))
        .or_else(|| toolchain_path(option_env!("RUSTUP_HOME").map(ToOwned::to_owned), option_env!("RUSTUP_TOOLCHAIN").map(ToOwned::to_owned)))
        .or_else(|| toolchain_path(option_env!("MULTIRUST_HOME").map(ToOwned::to_owned), option_env!("MULTIRUST_TOOLCHAIN").map(ToOwned::to_owned)))
}

pub fn main() {
    let mut args = env::args().collect::<Vec<_>>();

    let rustc_wrapper = args.get(1).map(Path::new).and_then(Path::file_stem) == Some("rustc".as_ref());
    if rustc_wrapper { args.remove(1); }

    let sysroot_arg = args.iter().find(|arg| arg.starts_with("--sysroot="))
        .or_else(|| args.iter().position(|arg| arg == "--sysroot").and_then(|i| args.get(i + 1)));
    let sysroot = sysroot_arg.map(PathBuf::from)
        .or_else(|| fetch_sysroot())
        .map(|path| path.to_string_lossy().to_string())
        .expect("specify --sysroot argument or SYSROOT environment variable, or use rustup or multirust");

    if sysroot_arg.is_none() { args.extend(["--sysroot".to_owned(), sysroot]); }

    // Make `mutest-driver --rustc` work like a subcommand that passes further args to rustc. For example
    // `mutest-driver --rustc --version` will print the rustc version that mutest-driver uses.
    if let Some(marker_arg_position) = args.iter().position(|arg| arg == "--rustc") {
        args.remove(marker_arg_position);
        args[0] = "rustc".to_string();

        process::exit(rustc_driver::catch_with_exit_code(|| {
            rustc_driver::RunCompiler::new(&args, &mut DefaultCallbacks).run()
        }));
    }

    let primary_package = env::var("CARGO_PRIMARY_PACKAGE").is_ok();
    let test_target = args.iter().any(|arg| arg.starts_with("--test"));
    let normal_rustc = args.iter().any(|arg| arg.starts_with("--print"));

    let mutest_args = (!rustc_wrapper)
        .then_some(args.iter().skip(1).map(ToOwned::to_owned).collect::<Vec<_>>().join(" "))
        .or_else(|| env::var("MUTEST_ARGS").ok());

    if normal_rustc || !primary_package || !test_target {
        process::exit(rustc_driver::catch_with_exit_code(|| {
            rustc_driver::RunCompiler::new(&args, &mut RustcCallbacks { mutest_args }).run()
        }));
    }

    let mutest_arg_matches = mutest_driver::cli::command()
        .no_binary_name(true)
        .get_matches_from(mutest_args.as_deref().unwrap_or_default().split(" "));

    process::exit(rustc_driver::catch_with_exit_code(|| {
        let compiler_config = mutest_driver::passes::parse_compiler_args(&args)?.expect("no compiler configuration was generated");

        let mutest_search_path = env::var("MUTEST_SEARCH_PATH").ok().map(PathBuf::from)
            .or_else(|| env::current_dir().ok().map(|v| v.join("target").join("debug")))
            .expect("specify MUTEST_SEARCH_PATH environment variable");

        let mode = match mutest_arg_matches.subcommand() {
            Some(("print-targets", _)) => config::Mode::PrintMutationTargets,
            Some(("print-mutants", _)) => config::Mode::PrintMutants,
            Some(("print-code", _)) => config::Mode::PrintCode,
            Some(("build", _)) => config::Mode::Build,
            _ => unreachable!(),
        };

        let unsafe_targeting = match () {
            _ if mutest_arg_matches.is_present("safe") => UnsafeTargeting::None,
            _ if mutest_arg_matches.is_present("cautious") => UnsafeTargeting::OnlyEnclosing(Unsafety::Unsafe),
            _ if mutest_arg_matches.is_present("unsafe") => UnsafeTargeting::All,
            _ => UnsafeTargeting::OnlyEnclosing(Unsafety::Normal),
        };

        let mutation_depth = mutest_arg_matches.value_of_t("depth").unwrap();
        let mutant_max_mutations_count = mutest_arg_matches.value_of_t("mutant-batch-size").unwrap();

        let config = Config {
            compiler_config,
            invocation_fingerprint: mutest_args,
            mutest_search_path,
            opts: config::Options {
                mode,
                unsafe_targeting,
                operators: &[
                    &mutest_operators::ArgDefaultShadow,
                    &mutest_operators::BitOpOrAndSwap,
                    &mutest_operators::BitOpOrXorSwap,
                    &mutest_operators::BitOpShiftDirSwap,
                    &mutest_operators::BitOpXorAndSwap,
                    &mutest_operators::BoolExprNegate,
                    &mutest_operators::CallDelete { limit_scope_to_local_callees: false },
                    &mutest_operators::CallValueDefaultShadow { limit_scope_to_local_callees: false },
                    &mutest_operators::ContinueBreakSwap,
                    &mutest_operators::EqOpInvert,
                    &mutest_operators::LogicalOpAndOrSwap,
                    &mutest_operators::OpAddMulSwap,
                    &mutest_operators::OpAddSubSwap,
                    &mutest_operators::OpDivRemSwap,
                    &mutest_operators::OpMulDivSwap,
                    &mutest_operators::RangeLimitSwap,
                    &mutest_operators::RelationalOpEqSwap,
                    &mutest_operators::RelationalOpInvert,
                ],
                mutation_depth,
                mutant_max_mutations_count,
            },
        };

        mutest_driver::run(config)?;
        Ok(())
    }));
}
