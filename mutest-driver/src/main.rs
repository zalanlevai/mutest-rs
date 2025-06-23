#![feature(decl_macro)]
#![feature(let_chains)]

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
use mutest_emit::analysis::hir::Safety;
use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_hash::FxHashSet;
use rustc_interface::Config as CompilerConfig;
use rustc_session::EarlyDiagCtxt;

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
            rustc_driver::run_compiler(&args, &mut DefaultCallbacks)
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
            rustc_driver::run_compiler(&args, &mut RustcCallbacks { mutest_args })
        }));
    }

    let mutest_arg_matches = mutest_driver_cli::command()
        .no_binary_name(true)
        .get_matches_from(mutest_args.as_deref().unwrap_or_default().split(" "));

    process::exit(rustc_driver::catch_with_exit_code(|| {
        let compiler_config = mutest_driver::passes::parse_compiler_args(&args).expect("no compiler configuration was generated");

        let early_dcx = EarlyDiagCtxt::new(compiler_config.opts.error_format);

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
            use mutest_driver_cli::print as opts;

            let mut print_names = mutest_arg_matches.get_many::<String>("print").map(|print| print.map(String::as_str).collect::<FxHashSet<_>>()).unwrap_or_default();
            if print_names.contains("all") { print_names = FxHashSet::from_iter(opts::ALL.into_iter().map(|s| *s)); }

            let mut print_opts = config::PrintOptions {
                print_headers: print_names.len() > 1,
                tests: None,
                mutation_targets: None,
                call_graph: None,
                conflict_graph: None,
                mutants: None,
                code: None,
            };

            let graph_format = {
                use mutest_driver_cli::graph_format as opts;

                match mutest_arg_matches.get_one::<String>("graph-format").map(String::as_str) {
                    Some(opts::SIMPLE) => config::GraphFormat::Simple,
                    Some(opts::GRAPHVIZ) => config::GraphFormat::Graphviz,
                    _ => unreachable!(),
                }
            };

            for print_name in print_names {
                match print_name {
                    opts::TESTS => print_opts.tests = Some(()),
                    opts::TARGETS => print_opts.mutation_targets = Some(()),
                    opts::CALL_GRAPH => {
                        let test_filters = mutest_arg_matches.get_many::<String>("call-graph-filter-tests").map(|s| s.map(|f| f.trim().to_owned()).collect::<Vec<_>>()).unwrap_or_default();
                        let non_local_call_view = {
                            use mutest_driver_cli::call_graph_non_local_call_view as opts;
                            match mutest_arg_matches.get_one::<String>("call-graph-non-local-calls").map(String::as_str) {
                                Some(opts::COLLAPSE) => config::CallGraphNonLocalCallView::Collapse,
                                Some(opts::EXPAND) => config::CallGraphNonLocalCallView::Expand,
                                _ => unreachable!(),
                            }
                        };
                        print_opts.call_graph = Some(config::CallGraphOptions { format: graph_format, test_filters, non_local_call_view });
                    }
                    opts::CONFLICT_GRAPH | opts::COMPATIBILITY_GRAPH => {
                        let compatibility_graph = matches!(print_name, opts::COMPATIBILITY_GRAPH);
                        let exclude_unsafe = mutest_arg_matches.get_flag("graph-exclude-unsafe");
                        print_opts.conflict_graph = Some(config::ConflictGraphOptions { compatibility_graph, exclude_unsafe, format: graph_format });
                    }
                    opts::MUTANTS => print_opts.mutants = Some(()),
                    opts::CODE => print_opts.code = Some(()),
                    _ => unreachable!("invalid print information name: `{print_name}`"),
                }
            }

            print_opts
        };

        let unsafe_targeting = match () {
            _ if mutest_arg_matches.get_flag("safe") => UnsafeTargeting::None,
            _ if mutest_arg_matches.get_flag("cautious") => UnsafeTargeting::OnlyEnclosing(Safety::Unsafe),
            _ if mutest_arg_matches.get_flag("risky") => UnsafeTargeting::OnlyEnclosing(Safety::Safe),
            _ if mutest_arg_matches.get_flag("unsafe") => UnsafeTargeting::All,
            _ => UnsafeTargeting::None,
        };

        let mutation_operators = {
            use mutest_driver_cli::mutation_operators as opts;

            let mut op_names = mutest_arg_matches.get_many::<String>("mutation-operators").unwrap().map(String::as_str).collect::<FxHashSet<_>>();
            if op_names.contains("all") { op_names = FxHashSet::from_iter(opts::ALL.into_iter().map(|s| *s)); }

            op_names.into_iter()
                .map(|op_name| {
                    macro const_op_ref($m:expr) { { const OP: Operators<'_, '_> = &[&$m]; OP[0] } }

                    match op_name {
                        opts::ARG_DEFAULT_SHADOW => const_op_ref!(mutest_operators::ArgDefaultShadow),
                        opts::BIT_OP_OR_AND_SWAP => const_op_ref!(mutest_operators::BitOpOrAndSwap),
                        opts::BIT_OP_OR_XOR_SWAP => const_op_ref!(mutest_operators::BitOpOrXorSwap),
                        opts::BIT_OP_SHIFT_DIR_SWAP => const_op_ref!(mutest_operators::BitOpShiftDirSwap),
                        opts::BIT_OP_XOR_AND_SWAP => const_op_ref!(mutest_operators::BitOpXorAndSwap),
                        opts::BOOL_EXPR_NEGATE => const_op_ref!(mutest_operators::BoolExprNegate),
                        opts::CALL_DELETE => const_op_ref!(mutest_operators::CallDelete { limit_scope_to_local_callees: false }),
                        opts::CALL_VALUE_DEFAULT_SHADOW => const_op_ref!(mutest_operators::CallValueDefaultShadow { limit_scope_to_local_callees: false }),
                        opts::CONTINUE_BREAK_SWAP => const_op_ref!(mutest_operators::ContinueBreakSwap),
                        opts::EQ_OP_INVERT => const_op_ref!(mutest_operators::EqOpInvert),
                        opts::LOGICAL_OP_AND_OR_SWAP => const_op_ref!(mutest_operators::LogicalOpAndOrSwap),
                        opts::MATH_OP_ADD_MUL_SWAP => const_op_ref!(mutest_operators::OpAddMulSwap),
                        opts::MATH_OP_ADD_SUB_SWAP => const_op_ref!(mutest_operators::OpAddSubSwap),
                        opts::MATH_OP_DIV_REM_SWAP => const_op_ref!(mutest_operators::OpDivRemSwap),
                        opts::MATH_OP_MUL_DIV_SWAP => const_op_ref!(mutest_operators::OpMulDivSwap),
                        opts::RANGE_LIMIT_SWAP => const_op_ref!(mutest_operators::RangeLimitSwap),
                        opts::RELATIONAL_OP_EQ_SWAP => const_op_ref!(mutest_operators::RelationalOpEqSwap),
                        opts::RELATIONAL_OP_INVERT => const_op_ref!(mutest_operators::RelationalOpInvert),
                        _ => unreachable!("invalid mutation operator name: `{op_name}`"),
                    }
                })
                .collect::<Vec<_>>()
        };

        let mut call_graph_depth_limit = mutest_arg_matches.get_one::<usize>("call-graph-depth-limit").copied();
        let mutation_depth = *mutest_arg_matches.get_one::<usize>("depth").unwrap();

        if let Some(call_graph_depth_limit_value) = call_graph_depth_limit && call_graph_depth_limit_value < mutation_depth {
            let mut diagnostic = early_dcx.early_struct_warn("explicit call graph depth limit argument ignored as mutation depth exceeds it");
            diagnostic.note(format!("mutation depth is set to {mutation_depth}"));
            diagnostic.note(format!("call graph depth limit was explicitly set to {call_graph_depth_limit_value}, but will be ignored"));
            diagnostic.emit();

            call_graph_depth_limit = None;
        }

        let mutation_batching_algorithm = {
            use mutest_driver_cli::mutant_batch_algorithm as opts;

            match mutest_arg_matches.get_one::<String>("mutant-batch-algorithm").map(String::as_str) {
                None | Some(opts::NONE) => config::MutationBatchingAlgorithm::None,

                Some(opts::RANDOM) => config::MutationBatchingAlgorithm::Random,

                Some(opts::GREEDY) => {
                    let ordering_heuristic = {
                        use mutest_driver_cli::mutant_batch_greedy_ordering_heuristic as opts;

                        match mutest_arg_matches.get_one::<String>("mutant-batch-greedy-ordering-heuristic").map(String::as_str) {
                            None | Some(opts::NONE) => None,
                            Some(opts::RANDOM) => Some(config::GreedyMutationBatchingOrderingHeuristic::Random),
                            Some(opts::CONFLICTS) => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsAsc),
                            Some(opts::REVERSE_CONFLICTS) => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsDesc),
                            _ => unreachable!(),
                        }
                    };

                    let epsilon = mutest_arg_matches.get_one::<f64>("mutant-batch-greedy-epsilon").copied();

                    config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, epsilon }
                }

                Some(opts::SIMULATED_ANNEALING) => config::MutationBatchingAlgorithm::SimulatedAnnealing,

                _ => unreachable!(),
            }
        };

        let mutation_batching_randomness = {
            use rand_seeder::Seeder;

            let seed_text = mutest_arg_matches.get_one::<String>("mutant-batch-seed");
            let seed = seed_text.map(|seed_text| Seeder::from(seed_text).make_seed::<config::RandomSeed>());

            config::MutationBatchingRandomness { seed }
        };

        let mutant_max_mutations_count = *mutest_arg_matches.get_one::<usize>("mutant-batch-size").unwrap();

        let write_opts = 'write_opts: {
            let Some(out_dir) = mutest_arg_matches.get_one::<PathBuf>("Zwrite-json").cloned() else {
                break 'write_opts None;
            };

            Some(config::WriteOptions { out_dir })
        };

        let verify_opts = {
            use mutest_driver_cli::verify as opts;

            let mut verify_opts = config::VerifyOptions {
                ast_lowering: false,
            };

            let mut verify_names = mutest_arg_matches.get_many::<String>("Zverify").map(|verify| verify.map(String::as_str).collect::<FxHashSet<_>>()).unwrap_or_default();
            if verify_names.contains("all") { verify_names = FxHashSet::from_iter(opts::ALL.into_iter().map(|s| *s)); }

            for verify_name in verify_names {
                match verify_name {
                    opts::AST_LOWERING => verify_opts.ast_lowering = true,
                    _ => unreachable!("invalid verify name: `{verify_name}`"),
                }
            }

            verify_opts
        };

        let sanitize_macro_expns = !mutest_arg_matches.get_flag("Zno-sanitize-macro-expns");

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
                call_graph_depth_limit,
                mutation_depth,
                mutation_batching_algorithm,
                mutation_batching_randomness,
                mutant_max_mutations_count,

                write_opts,
                verify_opts,
                sanitize_macro_expns,
            },
        };

        mutest_driver::run(config).unwrap();
    }));
}
