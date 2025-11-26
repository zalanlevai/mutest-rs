#![feature(array_windows)]
#![feature(decl_macro)]
#![feature(if_let_guard)]

#![feature(rustc_private)]
extern crate rustc_driver;
extern crate rustc_hash;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use mutest_driver::cargo_package_config;
use mutest_driver::config::{self, Config};
use mutest_driver::passes::external_mutant::RustcInvocation;
use mutest_emit::analysis::hir::Safety;
use mutest_emit::codegen::mutation::{OperatorRef, UnsafeTargeting};
use rustc_hash::FxHashSet;
use rustc_interface::Config as CompilerConfig;
use rustc_session::EarlyDiagCtxt;
use rustc_session::config::Input;

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
            mutest_driver::passes::track_invocation_fingerprint(parse_sess, args.as_deref());
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

enum TestType {
    UnitTest,
    IntegrationTest,
    Unknown,
}

// See `rustc_builtin_macros::test::test_type`.
fn guess_test_type(input: &Input) -> TestType {
    let input_file_path = input.opt_path().expect("cannot get input file path").to_owned();
    let input_dir_path = input_file_path.parent().unwrap_or(&input_file_path).to_owned();

    match () {
        _ if input_dir_path.ends_with("src") => TestType::UnitTest,
        _ if input_dir_path.ends_with("tests") => TestType::IntegrationTest,
        _ => TestType::Unknown,
    }
}

fn fetch_cargo_target_kind(input: &Input) -> Option<config::CargoTargetKind> {
    let cargo_package_manifest_path_str = env::var("CARGO_MANIFEST_PATH").ok()?;

    let mut metadata_cmd = cargo_metadata::MetadataCommand::new();
    metadata_cmd.manifest_path(&cargo_package_manifest_path_str);
    metadata_cmd.no_deps();

    let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");

    let Some(package) = metadata.packages.iter().find(|package| package.manifest_path == cargo_package_manifest_path_str) else {
        panic!("cannot find package in Cargo metadata");
    };

    let Some(cargo_package_name) = env::var("CARGO_PKG_NAME").ok() else {
        panic!("invalid Cargo invocation: missing `CARGO_PKG_NAME` environment variable");
    };
    let Some(cargo_crate_name) = env::var("CARGO_CRATE_NAME").ok() else {
        panic!("invalid Cargo invocation: missing `CARGO_CRATE_NAME` environment variable");
    };

    let input_file_path = input.opt_path().expect("cannot get input file path").to_owned();
    let input_file_path = input_file_path.canonicalize().expect("cannot canonicalize input file path");

    let Some(target) = package.targets.iter().find(|target| target.name.replace("-", "_") == cargo_crate_name && target.src_path == input_file_path) else {
        panic!("cannot find target in Cargo package metadata");
    };

    match () {
        _ if target.kind.contains(&cargo_metadata::TargetKind::Bin) => {
            // NOTE: We infer the main `bin` target based on whether its crate name matches the Cargo package name.
            match cargo_package_name.replace("-", "_") == cargo_crate_name {
                true => Some(config::CargoTargetKind::MainBin),
                false => Some(config::CargoTargetKind::Bin),
            }
        }
        // NOTE: We do not modify or analyze build scripts.
        _ if target.kind.contains(&cargo_metadata::TargetKind::CustomBuild) => None,
        // NOTE: We do not modify or analyze proc macro crates.
        _ if target.kind.contains(&cargo_metadata::TargetKind::ProcMacro) => None,

        _ if target.kind.contains(&cargo_metadata::TargetKind::Example) => Some(config::CargoTargetKind::Example),
        _ if target.kind.contains(&cargo_metadata::TargetKind::Test) => Some(config::CargoTargetKind::Test),
        // NOTE: Benchmarks are currently not supported.
        _ if target.kind.contains(&cargo_metadata::TargetKind::Bench) => None,

        // NOTE: Cargo allows only one `lib` target for each package, so there is no need for the "main" distinction like with `bin` targets.
        _ if target.kind.contains(&cargo_metadata::TargetKind::Lib)
            || target.kind.contains(&cargo_metadata::TargetKind::RLib)
            || target.kind.contains(&cargo_metadata::TargetKind::DyLib)
            || target.kind.contains(&cargo_metadata::TargetKind::CDyLib)
            || target.kind.contains(&cargo_metadata::TargetKind::StaticLib)
        => Some(config::CargoTargetKind::Lib),

        _ => None,
    }
}

mod crate_kind {
    mutest_driver_cli::exclusive_opts! { pub(crate) possible_values where
        INFER = "infer"; ["Infer crate kind based on the Cargo rustc invocation."]
        MUTANT_WITH_INTERNAL_TESTS = "mutant-with-internal-tests"; ["Crate mutated against its own internal test suite."]
        MUTABLE_DEP_FOR_EXTERNAL_TESTS = "mutable-dep-for-external-tests"; ["Crate that is used to create specialize mutants driven by an external test suite."]
        INTEGRATION_TESTS = "integration-tests"; ["External integration test crate that links against a specialized, mutated crate."]
    }
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
    let test_target = args.iter().any(|arg| arg == "--test")
        // NOTE: We attempt to mutate testing targets even if the libtest harness is disabled.
        //       This is required for supporting alternative test harnesses, such as embedded-test.
        //       If ultimately no tests are discovered, we simply generate an "empty" no-op meta-mutant.
        || (args.iter().any(|arg| arg == "--cfg=test") || args.array_windows().any(|[a, b]| a == "--cfg" && b == "test"));
    let normal_rustc = args.iter().any(|arg| arg.starts_with("--print"));

    let bin_target = args.iter().any(|arg| arg == "--crate-type=bin")
        || args.iter().position(|arg| arg == "--crate-type").is_some_and(|i| args.get(i + 1).is_some_and(|v| v == "bin"));

    let mutest_args = (!rustc_wrapper)
        .then_some(args.iter().skip(1).map(ToOwned::to_owned).collect::<Vec<_>>().join(" "))
        .or_else(|| env::var("MUTEST_ARGS").ok());

    if normal_rustc || !primary_package || (bin_target && !test_target) {
        process::exit(rustc_driver::catch_with_exit_code(|| {
            rustc_driver::run_compiler(&args, &mut RustcCallbacks { mutest_args })
        }));
    }

    let mutest_arg_matches = mutest_driver_cli::command()
        .no_binary_name(true)
        // Target-related Arguments
        .arg(clap::arg!(--"crate-kind" [CRATE_KIND] "Determine how the crate is handled in terms of mutations and tests.").value_parser(crate_kind::possible_values()).default_value(crate_kind::INFER).display_order(200))
        .get_matches_from(mutest_args.as_deref().unwrap_or_default().split(" "));

    process::exit(rustc_driver::catch_with_exit_code(|| {
        let compiler_config = mutest_driver::passes::parse_compiler_args(&args).expect("no compiler configuration was generated");

        let early_dcx = EarlyDiagCtxt::new(compiler_config.opts.error_format);

        let cargo_target_kind = fetch_cargo_target_kind(&compiler_config.input);

        let mut package_config = cargo_package_config::fetch_merged_cargo_package_config(&early_dcx);

        let crate_kind_arg = mutest_arg_matches.get_one::<String>("crate-kind").map(String::as_str);
        if !test_target || crate_kind_arg == Some(crate_kind::MUTABLE_DEP_FOR_EXTERNAL_TESTS) {
            let report_timings = mutest_arg_matches.get_flag("timings");

            let rustc_invocation = RustcInvocation {
                args: args.iter().cloned().collect::<Vec<_>>(),
                env_vars: env::vars().collect::<Vec<_>>(),
            };

            let compilation_pass = mutest_driver::passes::external_mutant::recompilable_dep_crate::compile_recompilable_dep_crate(&compiler_config, &rustc_invocation).unwrap();

            if report_timings {
                println!("compilation took {compilation:.2?}",
                    compilation = compilation_pass.duration,
                );
            }
            return;
        }

        let mutest_target_dir_root = env::var("MUTEST_TARGET_DIR_ROOT").ok().map(PathBuf::from);
        let mutest_search_path = env::var("MUTEST_SEARCH_PATH").ok().map(PathBuf::from);

        let crate_kind = {
            use crate::crate_kind as opts;

            match crate_kind_arg {
                Some(opts::MUTANT_WITH_INTERNAL_TESTS) => config::CrateKind::MutantWithInternalTests,
                Some(opts::MUTABLE_DEP_FOR_EXTERNAL_TESTS) => unreachable!(),
                Some(opts::INTEGRATION_TESTS) => config::CrateKind::IntegrationTest,

                None | Some(opts::INFER) => match cargo_target_kind {
                    None => match guess_test_type(&compiler_config.input) {
                        TestType::UnitTest | TestType::Unknown => config::CrateKind::MutantWithInternalTests,
                        TestType::IntegrationTest => config::CrateKind::IntegrationTest,
                    },
                    Some(config::CargoTargetKind::Test) => config::CrateKind::IntegrationTest,
                    Some(_) => config::CrateKind::MutantWithInternalTests,
                }

                _ => unreachable!(),
            }
        };

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
                mutations: None,
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
                        let entry_point_filters = mutest_arg_matches.get_many::<String>("call-graph-filter-entry-points").map(|s| s.map(|f| f.trim().to_owned()).collect::<Vec<_>>()).unwrap_or_default();
                        let non_local_call_view = {
                            use mutest_driver_cli::call_graph_non_local_call_view as opts;
                            match mutest_arg_matches.get_one::<String>("call-graph-non-local-calls").map(String::as_str) {
                                Some(opts::COLLAPSE) => config::CallGraphNonLocalCallView::Collapse,
                                Some(opts::EXPAND) => config::CallGraphNonLocalCallView::Expand,
                                _ => unreachable!(),
                            }
                        };
                        print_opts.call_graph = Some(config::CallGraphOptions { format: graph_format, entry_point_filters, non_local_call_view });
                    }
                    opts::CONFLICT_GRAPH | opts::COMPATIBILITY_GRAPH => {
                        let compatibility_graph = matches!(print_name, opts::COMPATIBILITY_GRAPH);
                        let exclude_unsafe = mutest_arg_matches.get_flag("graph-exclude-unsafe");
                        print_opts.conflict_graph = Some(config::ConflictGraphOptions { compatibility_graph, exclude_unsafe, format: graph_format });
                    }
                    opts::MUTATIONS => print_opts.mutations = Some(()),
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

        let mutation_operators = 'mutation_operators: {
            use mutest_driver_cli::mutation_operators as opts;

            if let None | Some(clap::parser::ValueSource::DefaultValue) = mutest_arg_matches.value_source("mutation-operators") {
                if let Some(c) = &mut package_config && let Some(mutation_operators) = c.mutation_operators.take() {
                    break 'mutation_operators mutation_operators;
                }
            }

            let mut op_names = mutest_arg_matches.get_many::<String>("mutation-operators").unwrap().map(String::as_str).collect::<Vec<_>>();
            if op_names.contains(&"all") { op_names = opts::ALL.into_iter().map(|s| *s).collect::<Vec<_>>(); }
            // NOTE: Mutation operators must be sorted into a deterministic order,
            //       because that determines application order, and thus mutation order.
            //       We use an alphabetical order for this based on operator names.
            op_names.sort();
            op_names.dedup();

            op_names.into_iter()
                .map(|op_name| {
                    macro const_op_ref($m:expr) { { const OP: OperatorRef<'_, '_> = &$m; OP } }

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

        let mut call_graph_depth_limit = match mutest_arg_matches.value_source("call-graph-depth-limit") {
            None | Some(clap::parser::ValueSource::DefaultValue) if let Some(c) = &package_config && let Some(v) = c.call_graph_depth_limit => Some(v),
            _ => mutest_arg_matches.get_one::<usize>("call-graph-depth-limit").copied(),
        };
        let mut call_graph_trace_length_limit = match mutest_arg_matches.value_source("call-graph-trace-length-limit") {
            None | Some(clap::parser::ValueSource::DefaultValue) if let Some(c) = &package_config && let Some(v) = c.call_graph_trace_length_limit => Some(v),
            _ => mutest_arg_matches.get_one::<usize>("call-graph-trace-length-limit").copied(),
        };
        let mutation_depth = match mutest_arg_matches.value_source("depth") {
            None | Some(clap::parser::ValueSource::DefaultValue) if let Some(c) = &package_config && let Some(v) = c.mutation_depth => v,
            _ => *mutest_arg_matches.get_one::<usize>("depth").unwrap(),
        };

        if let Some(call_graph_depth_limit_value) = call_graph_depth_limit && call_graph_depth_limit_value < mutation_depth {
            let mut diagnostic = early_dcx.early_struct_warn("explicit call graph depth limit option ignored as mutation depth exceeds it");
            diagnostic.note(format!("mutation depth is set to {mutation_depth}"));
            diagnostic.note(format!("call graph depth limit was explicitly set to {call_graph_depth_limit_value}, but will be ignored"));
            diagnostic.emit();

            call_graph_depth_limit = None;
        }

        if let Some(call_graph_trace_length_limit_value) = call_graph_trace_length_limit && call_graph_trace_length_limit_value < mutation_depth {
            let mut diagnostic = early_dcx.early_struct_warn("explicit call graph trace length limit option ignored as mutation depth exceeds it");
            diagnostic.note(format!("mutation depth is set to {mutation_depth}"));
            diagnostic.note(format!("call graph trace length limit was explicitly set to {call_graph_trace_length_limit_value}, but will be ignored"));
            diagnostic.emit();

            call_graph_trace_length_limit = None;
        }

        let mutation_parallelism = 'mutation_parallelism: {
            let mutation_parallelism_config = package_config.as_ref().and_then(|c| c.mutation_parallelism.as_ref());

            use mutest_driver_cli::mutant_batch_algorithm as opts;
            let batching_algorithm_arg = mutest_arg_matches.get_one::<String>("mutant-batch-algorithm").map(String::as_str);

            match (batching_algorithm_arg, mutest_arg_matches.value_source("mutant-batch-algorithm"), mutation_parallelism_config) {
                // Mutation batching is overriden through Cargo package config.
                (_, None | Some(clap::parser::ValueSource::DefaultValue), Some(mutation_parallelism_config)) => {
                    match mutation_parallelism_config {
                        // Mutation batching is explicitly disabled through Cargo package config.
                        cargo_package_config::MutationParallelism::None => break 'mutation_parallelism None,
                        // Mutation batching is explicitly enabled through Cargo package config.
                        cargo_package_config::MutationParallelism::Batching { .. } => {}
                    }
                }

                // Mutation batching algorith is disabled through the CLI, either explicitly or through the defaults.
                (None | Some(opts::NONE), _, _) => break 'mutation_parallelism None,
                // Some mutation batching algorithm is enabled through the CLI.
                _ => {}
            }

            let batching_algorithm = {
                let batching_algorithm_config = match mutation_parallelism_config {
                    Some(cargo_package_config::MutationParallelism::Batching { batching_algorithm: Some(v), .. }) => Some(v),
                    _ => None,
                };

                match (batching_algorithm_arg, batching_algorithm_config) {
                    (Some(opts::RANDOM), _) | (_, Some(cargo_package_config::MutationBatchingAlgorithm::Random))
                    => config::MutationBatchingAlgorithm::Random,

                    (Some(opts::GREEDY), _) | (_, Some(cargo_package_config::MutationBatchingAlgorithm::Greedy { .. })) => {
                        let ordering_heuristic = 'ordering_heuristic: {
                            use mutest_driver_cli::mutant_batch_greedy_ordering_heuristic as opts;

                            let ordering_heuristic_config = match batching_algorithm_config {
                                Some(cargo_package_config::MutationBatchingAlgorithm::Greedy { greedy_batching_ordering_heuristic: v, .. }) => v.as_ref(),
                                _ => None,
                            };

                            if let None | Some(clap::parser::ValueSource::DefaultValue) = mutest_arg_matches.value_source("mutant-batch-greedy-ordering-heuristic") {
                                if let Some(ordering_heuristic_config) = ordering_heuristic_config {
                                    break 'ordering_heuristic match ordering_heuristic_config {
                                        cargo_package_config::GreedyMutationBatchingOrderingHeuristic::None => None,
                                        cargo_package_config::GreedyMutationBatchingOrderingHeuristic::Random => Some(config::GreedyMutationBatchingOrderingHeuristic::Random),
                                        cargo_package_config::GreedyMutationBatchingOrderingHeuristic::Conflicts => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsAsc),
                                        cargo_package_config::GreedyMutationBatchingOrderingHeuristic::ReverseConflicts => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsDesc),
                                    };
                                }
                            }

                            match mutest_arg_matches.get_one::<String>("mutant-batch-greedy-ordering-heuristic").map(String::as_str) {
                                None | Some(opts::NONE) => None,
                                Some(opts::RANDOM) => Some(config::GreedyMutationBatchingOrderingHeuristic::Random),
                                Some(opts::CONFLICTS) => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsAsc),
                                Some(opts::REVERSE_CONFLICTS) => Some(config::GreedyMutationBatchingOrderingHeuristic::ConflictsDesc),
                                _ => unreachable!(),
                            }
                        };

                        let epsilon = match mutest_arg_matches.value_source("mutant-batch-greedy-epsilon") {
                            None | Some(clap::parser::ValueSource::DefaultValue) if let Some(cargo_package_config::MutationBatchingAlgorithm::Greedy { greedy_batching_epsilon: Some(v), .. }) = batching_algorithm_config => Some(*v),
                            _ => mutest_arg_matches.get_one::<f64>("mutant-batch-greedy-epsilon").copied(),
                        };

                        config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, epsilon }
                    }

                    (Some(opts::SIMULATED_ANNEALING), _)
                    => config::MutationBatchingAlgorithm::SimulatedAnnealing,

                    _ => unreachable!(),
                }
            };

            let batching_randomness = {
                use rand_seeder::Seeder;

                let seed_text = mutest_arg_matches.get_one::<String>("mutant-batch-seed").or_else(|| match mutation_parallelism_config {
                    Some(cargo_package_config::MutationParallelism::Batching { batching_seed: Some(v), .. }) => Some(v),
                    _ => None,
                });
                let seed = seed_text.map(|seed_text| Seeder::from(seed_text).make_seed::<config::RandomSeed>());

                config::MutationBatchingRandomness { seed }
            };

            let batch_max_mutations_count = match mutest_arg_matches.value_source("mutant-batch-size") {
                None | Some(clap::parser::ValueSource::DefaultValue) if let Some(cargo_package_config::MutationParallelism::Batching { batch_size: Some(v), .. }) = mutation_parallelism_config => *v,
                _ => *mutest_arg_matches.get_one::<usize>("mutant-batch-size").unwrap(),
            };

            Some(config::MutationParallelism::Batching(config::MutationBatchingOptions {
                algorithm: batching_algorithm,
                randomness: batching_randomness,
                batch_max_mutations_count,
            }))
        };

        let write_opts = 'write_opts: {
            let Some(clap::parser::ValueSource::CommandLine) = mutest_arg_matches.value_source("Zwrite-json") else {
                break 'write_opts None;
            };

            let mut out_dir = mutest_arg_matches.get_one::<PathBuf>("Zwrite-json").cloned()
                .unwrap_or_else(|| mutest_target_dir_root.clone().unwrap_or(PathBuf::from("target/mutest")).join("json"));

            if let Some(cargo_package_name) = env::var("CARGO_PKG_NAME").ok() {
                // NOTE: `CARGO_PKG_NAME` always corresponds to the root package name, even for other targets.
                out_dir.push(cargo_package_name);

                let Some(cargo_crate_name) = env::var("CARGO_CRATE_NAME").ok() else {
                    panic!("invalid Cargo invocation: missing `CARGO_CRATE_NAME` environment variable");
                };

                match cargo_target_kind {
                    None => {}

                    Some(config::CargoTargetKind::Lib) => out_dir.push("lib"),
                    Some(config::CargoTargetKind::MainBin) => out_dir.push("bin"),

                    Some(config::CargoTargetKind::Bin) => {
                        out_dir.push("bins");
                        out_dir.push(cargo_crate_name);
                    }
                    Some(config::CargoTargetKind::Example) => {
                        out_dir.push("examples");
                        out_dir.push(cargo_crate_name);
                    }
                    Some(config::CargoTargetKind::Test) => {
                        out_dir.push("tests");
                        out_dir.push(cargo_crate_name);
                    }
                }

                fs::create_dir_all(&out_dir).expect(&format!("cannot create JSON output directory for crate at `{}`", out_dir.display()));
            }

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

        let embedded = mutest_arg_matches.get_flag("Zembedded");

        let sanitize_macro_expns = !mutest_arg_matches.get_flag("Zno-sanitize-macro-expns");

        let config = Config {
            compiler_config,
            invocation_fingerprint: mutest_args,
            mutest_target_dir_root,
            mutest_search_path,
            opts: config::Options {
                crate_kind,
                cargo_target_kind,

                mode,
                verbosity,
                report_timings,
                print_opts,
                unsafe_targeting,
                operators: &mutation_operators,
                call_graph_depth_limit,
                call_graph_trace_length_limit,
                mutation_depth,
                mutation_parallelism,

                write_opts,
                verify_opts,
                embedded,
                sanitize_macro_expns,
            },
        };

        mutest_driver::run(config).unwrap();
    }));
}
