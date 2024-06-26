#![feature(decl_macro)]

#![feature(rustc_private)]
extern crate rustc_driver;
extern crate rustc_hash;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

use std::env;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use mutest_driver::config::{self, Config};
use mutest_emit::analysis::hir::Unsafety;
use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_hash::FxHashSet;
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
        config.psess_created = Some(Box::new(move |parse_sess| {
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

    let mutest_arg_matches = mutest_driver_cli::command()
        .no_binary_name(true)
        .get_matches_from(mutest_args.as_deref().unwrap_or_default().split(" "));

    process::exit(rustc_driver::catch_with_exit_code(|| {
        let compiler_config = mutest_driver::passes::parse_compiler_args(&args)?.expect("no compiler configuration was generated");

        let mutest_search_path = env::var("MUTEST_SEARCH_PATH").ok().map(PathBuf::from)
            .or_else(|| env::current_dir().ok().map(|v| v.join("target").join("debug")))
            .expect("specify MUTEST_SEARCH_PATH environment variable");

        let mode = match mutest_arg_matches.subcommand() {
            Some(("print", _)) => config::Mode::Print,
            Some(("build", _)) => config::Mode::Build,
            _ => unreachable!(),
        };

        let verbosity = mutest_arg_matches.get_count("verbose");
        let report_timings = mutest_arg_matches.get_flag("timings");

        let print_opts = {
            use mutest_driver_cli::print::*;

            let mut print_names = mutest_arg_matches.get_many::<String>("print").map(|print| print.map(String::as_str).collect::<FxHashSet<_>>()).unwrap_or_default();
            if print_names.contains("all") { print_names = FxHashSet::from_iter(ALL.into_iter().map(|s| *s)); }

            let mut print_opts = config::PrintOptions {
                print_headers: print_names.len() > 1,
                mutation_targets: None,
                conflict_graph: None,
                mutants: None,
                code: None,
            };

            for print_name in print_names {
                match print_name {
                    TARGETS => print_opts.mutation_targets = Some(()),
                    CONFLICT_GRAPH | COMPATIBILITY_GRAPH => {
                        use mutest_driver_cli::graph_format::*;

                        let compatibility_graph = matches!(print_name, COMPATIBILITY_GRAPH);
                        let exclude_unsafe = mutest_arg_matches.get_flag("graph-exclude-unsafe");
                        let format = match mutest_arg_matches.get_one::<String>("graph-format").map(String::as_str) {
                            Some(SIMPLE) => config::GraphFormat::Simple,
                            Some(GRAPHVIZ) => config::GraphFormat::Graphviz,
                            _ => unreachable!(),
                        };
                        print_opts.conflict_graph = Some(config::ConflictGraphOptions { compatibility_graph, exclude_unsafe, format });
                    }
                    MUTANTS => print_opts.mutants = Some(()),
                    CODE => print_opts.code = Some(()),
                    _ => unreachable!("invalid print information name: `{print_name}`"),
                }
            }

            print_opts
        };

        let unsafe_targeting = match () {
            _ if mutest_arg_matches.get_flag("safe") => UnsafeTargeting::None,
            _ if mutest_arg_matches.get_flag("cautious") => UnsafeTargeting::OnlyEnclosing(Unsafety::Unsafe),
            _ if mutest_arg_matches.get_flag("risky") => UnsafeTargeting::OnlyEnclosing(Unsafety::Normal),
            _ if mutest_arg_matches.get_flag("unsafe") => UnsafeTargeting::All,
            _ => UnsafeTargeting::None,
        };

        let mutation_operators = {
            use mutest_driver_cli::mutation_operators::*;

            let mut op_names = mutest_arg_matches.get_many::<String>("mutation-operators").unwrap().map(String::as_str).collect::<FxHashSet<_>>();
            if op_names.contains("all") { op_names = FxHashSet::from_iter(ALL.into_iter().map(|s| *s)); }

            op_names.into_iter()
                .map(|op_name| {
                    macro const_op_ref($m:expr) { { const OP: Operators<'_, '_> = &[&$m]; OP[0] } }

                    match op_name {
                        ARG_DEFAULT_SHADOW => const_op_ref!(mutest_operators::ArgDefaultShadow),
                        BIT_OP_OR_AND_SWAP => const_op_ref!(mutest_operators::BitOpOrAndSwap),
                        BIT_OP_OR_XOR_SWAP => const_op_ref!(mutest_operators::BitOpOrXorSwap),
                        BIT_OP_SHIFT_DIR_SWAP => const_op_ref!(mutest_operators::BitOpShiftDirSwap),
                        BIT_OP_XOR_AND_SWAP => const_op_ref!(mutest_operators::BitOpXorAndSwap),
                        BOOL_EXPR_NEGATE => const_op_ref!(mutest_operators::BoolExprNegate),
                        CALL_DELETE => const_op_ref!(mutest_operators::CallDelete { limit_scope_to_local_callees: false }),
                        CALL_VALUE_DEFAULT_SHADOW => const_op_ref!(mutest_operators::CallValueDefaultShadow { limit_scope_to_local_callees: false }),
                        CONTINUE_BREAK_SWAP => const_op_ref!(mutest_operators::ContinueBreakSwap),
                        EQ_OP_INVERT => const_op_ref!(mutest_operators::EqOpInvert),
                        LOGICAL_OP_AND_OR_SWAP => const_op_ref!(mutest_operators::LogicalOpAndOrSwap),
                        MATH_OP_ADD_MUL_SWAP => const_op_ref!(mutest_operators::OpAddMulSwap),
                        MATH_OP_ADD_SUB_SWAP => const_op_ref!(mutest_operators::OpAddSubSwap),
                        MATH_OP_DIV_REM_SWAP => const_op_ref!(mutest_operators::OpDivRemSwap),
                        MATH_OP_MUL_DIV_SWAP => const_op_ref!(mutest_operators::OpMulDivSwap),
                        RANGE_LIMIT_SWAP => const_op_ref!(mutest_operators::RangeLimitSwap),
                        RELATIONAL_OP_EQ_SWAP => const_op_ref!(mutest_operators::RelationalOpEqSwap),
                        RELATIONAL_OP_INVERT => const_op_ref!(mutest_operators::RelationalOpInvert),
                        _ => unreachable!("invalid mutation operator name: `{op_name}`"),
                    }
                })
                .collect::<Vec<_>>()
        };

        let call_graph_depth = *mutest_arg_matches.get_one::<usize>("call-graph-depth").unwrap();
        let mutation_depth = *mutest_arg_matches.get_one::<usize>("depth").unwrap();

        let mutation_batching_algorithm = {
            use mutest_driver_cli::mutant_batch_algorithm::*;

            match mutest_arg_matches.get_one::<String>("mutant-batch-algorithm").map(String::as_str) {
                None | Some(NONE) => config::MutationBatchingAlgorithm::None,

                Some(GREEDY) => {
                    let ordering_heuristic = {
                        use mutest_driver_cli::mutant_batch_greedy_ordering_heuristic::*;

                        match mutest_arg_matches.get_one::<String>("mutant-batch-greedy-ordering-heuristic").map(String::as_str) {
                            None | Some(NONE) => None,
                            Some(CONFLICTS) => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsAsc),
                            Some(REVERSE_CONFLICTS) => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsDesc),

                            #[cfg(feature = "random")]
                            Some(RANDOM) => Some(config::GreedyMutationBatchingOrderingHeuristic::Random),

                            _ => unreachable!(),
                        }
                    };

                    #[cfg(feature = "random")]
                    let epsilon = mutest_arg_matches.get_one::<f64>("mutant-batch-greedy-epsilon").copied();

                    config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, #[cfg(feature = "random")] epsilon }
                }

                #[cfg(feature = "random")]
                Some(RANDOM) => config::MutationBatchingAlgorithm::Random,

                #[cfg(feature = "random")]
                Some(SIMULATED_ANNEALING) => config::MutationBatchingAlgorithm::SimulatedAnnealing,

                _ => unreachable!(),
            }
        };

        #[cfg(feature = "random")]
        let mutation_batching_randomness = {
            use rand_seeder::Seeder;

            let seed_text = mutest_arg_matches.get_one::<String>("mutant-batch-seed");
            let seed = seed_text.map(|seed_text| Seeder::from(seed_text).make_seed::<config::RandomSeed>());

            config::MutationBatchingRandomness { seed }
        };

        let mutant_max_mutations_count = *mutest_arg_matches.get_one::<usize>("mutant-batch-size").unwrap();

        let sanitize_macro_expns = mutest_arg_matches.get_flag("Zsanitize-macro-expns");

        let config = Config {
            compiler_config,
            invocation_fingerprint: mutest_args,
            mutest_search_path,
            opts: config::Options {
                mode,
                verbosity,
                report_timings,
                print_opts,
                unsafe_targeting,
                operators: &mutation_operators,
                call_graph_depth,
                mutation_depth,
                mutation_batching_algorithm,
                #[cfg(feature = "random")] mutation_batching_randomness,
                mutant_max_mutations_count,

                sanitize_macro_expns,
            },
        };

        mutest_driver::run(config)?;
        Ok(())
    }));
}
