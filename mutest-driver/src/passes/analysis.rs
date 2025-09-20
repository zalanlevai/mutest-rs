use std::env;
use std::time::{Duration, Instant};

use rustc_hash::{FxHashSet, FxHashMap};
use rustc_interface::{create_and_enter_global_ctxt, passes, run_compiler};
use rustc_interface::interface::Result as CompilerResult;
use rustc_middle::bug;
use rustc_middle::ty::TyCtxt;
use rustc_session::config::OptLevel;
use rustc_span::ErrorGuaranteed;
use rustc_span::edition::Edition;
use rustc_span::fatal_error::FatalError;
use mutest_emit::analysis::call_graph::{EntryPointAssocs, EntryPoints, Targeting, TargetReachability};
use mutest_emit::analysis::hir;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::harness::MetaMutant;
use mutest_emit::codegen::symbols::{Symbol, span_diagnostic_ord};

use crate::config::{self, Config};
use crate::inject::inject_runtime_crate_and_deps;
use crate::passes::{Flow, base_compiler_config};
use crate::passes::external_mutant::{ExternalTargets, StableTarget};
use crate::passes::external_mutant::crate_const_storage;
use crate::passes::external_mutant::specialized_crate::SpecializedMutantCrateCompilationRequest;
use crate::print::{print_call_graph, print_mutations, print_mutation_graph, print_targets, print_tests};
use crate::write::{write_call_graph, write_mutations, write_tests, write_timings};

pub struct AnalysisPassResult {
    pub duration: Duration,
    pub test_discovery_duration: Duration,
    pub target_analysis_duration: Duration,
    pub sanitize_macro_expns_duration: Duration,
    pub mutation_generation_duration: Duration,
    pub mutation_conflict_resolution_duration: Duration,
    pub mutation_batching_duration: Duration,
    pub codegen_duration: Duration,
    pub write_duration: Duration,
    pub generated_crate_code: String,
    pub specialized_external_mutant_crate: Option<(String, SpecializedMutantCrateCompilationRequest)>,
}

fn perform_codegen<'tcx, 'ent, 'trg, 'm>(
    tcx: TyCtxt<'tcx>,
    opts: &mut config::Options,
    pass_result: &mut AnalysisPassResult,
    generated_crate_ast: &mut ast::Crate,
    unexpanded_crate_ast: &mut ast::Crate,
    entry_points: EntryPoints<'ent>,
    meta_mutant: MetaMutant<'trg, 'm>,
) {
    // HACK: See below.
    mutest_emit::codegen::expansion::insert_generated_code_crate_refs(tcx, generated_crate_ast);
    mutest_emit::codegen::expansion::insert_generated_code_prelude_attrs(tcx, generated_crate_ast);

    if opts.crate_kind.provides_tests() {
        mutest_emit::codegen::entry_point::clean_entry_points(tcx.sess, generated_crate_ast);
        mutest_emit::codegen::entry_point::generate_dummy_main(tcx, generated_crate_ast);
    }

    // TODO: Deprecate and remove from expansion module.
    if !opts.sanitize_macro_expns {
        mutest_emit::codegen::expansion::load_modules(tcx.sess, unexpanded_crate_ast);
        mutest_emit::codegen::expansion::revert_non_local_macro_expansions(generated_crate_ast, unexpanded_crate_ast);
    }

    mutest_emit::codegen::substitution::resolve_syntax_ambiguities(tcx, generated_crate_ast);

    mutest_emit::codegen::harness::generate_harness(tcx, entry_points, meta_mutant, generated_crate_ast);

    // HACK: The generated code is currently based on the expanded AST and contains references to the internals
    //       of macro expansions. These are patched over using a static attribute prelude (here) and a static
    //       set of crate references (above).
    struct NoAnn;
    impl rustc_ast_pretty::pprust::state::PpAnn for NoAnn {}
    pass_result.generated_crate_code = format!("{prelude}\n{code}",
        prelude = mutest_emit::codegen::expansion::GENERATED_CODE_PRELUDE,
        code = rustc_ast_pretty::pprust::print_crate(
            tcx.sess.source_map(),
            generated_crate_ast,
            tcx.sess.io.input.source_name(),
            "".to_owned(),
            &NoAnn,
            true,
            tcx.sess.edition(),
            &tcx.sess.psess.attr_id_generator,
        ),
    );
}

pub fn run(config: &mut Config) -> CompilerResult<Option<AnalysisPassResult>> {
    let mut compiler_config = base_compiler_config(config);

    // Compile the crate in test-mode to access tests defined behind `#[cfg(test)]`.
    compiler_config.opts.test = config.opts.crate_kind.provides_tests();

    // NOTE: We need to inject the `mutest_runtime` crate and its dependencies during analysis
    //       for test crates that link against external meta-mutants because
    //       the meta-mutant crate references the `mutest_runtime` crate.
    if !config.opts.crate_kind.produces_mutations() {
        inject_runtime_crate_and_deps(config, &mut compiler_config, None);
    }

    // NOTE: We must turn off `format_args` optimizations to be able to match up
    //       argument nodes between the AST and the HIR.
    //       See `mutest_emit::analysis::ast_lowering` for more details.
    compiler_config.opts.unstable_opts.flatten_format_args = false;

    // NOTE: Disable all MIR optimizations in all cases to ensure identical MIRs
    //       during analysis regardless of final optimization level.
    compiler_config.opts.optimize = OptLevel::No;
    // NOTE: We must disable MIR optimizations to disable inlining of function calls,
    //       which is necessary to building a complete call graph in all circumstances.
    //       The MIR generated in this pass is never used for any compilation anyway.
    compiler_config.opts.unstable_opts.mir_opt_level = Some(0);

    let opts = &mut config.opts;

    let sess_opts = mutest_emit::session::Options {
        verbosity: opts.verbosity,
        report_timings: opts.report_timings,
        sanitize_macro_expns: opts.sanitize_macro_expns,
    };

    let analysis_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<Option<AnalysisPassResult>> {
        let t_start = Instant::now();
        let mut pass_result = AnalysisPassResult {
            duration: Duration::ZERO,
            test_discovery_duration: Duration::ZERO,
            target_analysis_duration: Duration::ZERO,
            sanitize_macro_expns_duration: Duration::ZERO,
            mutation_generation_duration: Duration::ZERO,
            mutation_conflict_resolution_duration: Duration::ZERO,
            mutation_batching_duration: Duration::ZERO,
            codegen_duration: Duration::ZERO,
            write_duration: Duration::ZERO,
            generated_crate_code: String::new(),
            specialized_external_mutant_crate: None,
        };

        let sess = &compiler.sess;

        let mut crate_ast = passes::parse(sess);
        // NOTE: We must register our custom tool attribute namespace before the
        //       relevant attribute validation is performed during macro expansion.
        mutest_emit::codegen::tool_attr::register(sess, &mut crate_ast);

        if sess.edition() == Edition::Edition2015 {
            let mut diagnostic = sess.dcx().struct_warn("edition 2015 code is not explicitly supported");
            diagnostic.note("you may encounter issues with generated code from macro expansion sanitization and mutation operators");
            diagnostic.emit();
        }

        let result = create_and_enter_global_ctxt(compiler, crate_ast.clone(), |tcx| -> Flow<AnalysisPassResult, ErrorGuaranteed> {
            let (mut generated_crate_ast, def_res) = {
                let (resolver, expanded_crate_ast) = &*tcx.resolver_for_lowering().borrow();
                let def_res = mutest_emit::analysis::ast_lowering::DefResolutions::from_resolver(resolver);

                // TODO: Generate code based on the original, unexpanded AST instead of the
                //       expanded AST which may contain invalid code that is not equivalent due
                //       to macro hygiene.
                let generated_crate_ast = (**expanded_crate_ast).clone();

                (generated_crate_ast, def_res)
            };

            let t_test_discovery_start = Instant::now();
            let tests = opts.crate_kind.provides_tests()
                .then(|| mutest_emit::analysis::tests::collect_tests(&generated_crate_ast, &def_res))
                .unwrap_or_default();
            pass_result.test_discovery_duration = t_test_discovery_start.elapsed();

            if let Some(write_opts) = &opts.write_opts && opts.crate_kind.provides_tests() {
                let t_write_start = Instant::now();
                write_tests(write_opts, tcx, &tests, pass_result.test_discovery_duration);
                pass_result.write_duration += t_write_start.elapsed();
            }

            if let Some(_) = opts.print_opts.tests.take() && opts.crate_kind.provides_tests() {
                if opts.print_opts.print_headers { println!("\n@@@ tests @@@\n"); }
                print_tests(&tests);
                if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                    if let Some(write_opts) = &opts.write_opts {
                        pass_result.duration = t_start.elapsed();
                        write_timings(write_opts, t_start.elapsed(), &pass_result, None, None);
                    }
                    if opts.report_timings {
                        println!("\nfinished in {total:.2?} (write {write:.2?})",
                            total = t_start.elapsed(),
                            write = pass_result.write_duration,
                        );
                    }
                    return Flow::Break;
                }
                if opts.verbosity >= 1 { println!(); }
            }

            tcx.ensure_ok().analysis(());

            let crate_res = mutest_emit::analysis::res::CrateResolutions::from_post_analysis_tcx(tcx);

            let external_meta_mutant_crate = (!opts.crate_kind.produces_mutations()).then(|| {
                let extern_crates_with_mutest_harnesses = tcx.crates(()).iter().copied()
                    .filter(|&cnum| mutest_emit::codegen::harness::find_harness_in_extern_crate(tcx, cnum).is_some())
                    .collect::<Vec<_>>();

                match &extern_crates_with_mutest_harnesses[..] {
                    &[cnum] => cnum,

                    [] => tcx.dcx().fatal("missing extern mutant crate for integration test"),
                    cnums => {
                        let mut diagnostic = tcx.dcx().struct_fatal("found multiple extern mutant crate candidates");

                        if let Some(cargo_package_name) = env::var("CARGO_PKG_NAME").ok() {
                            let crate_matching_cargo_package_name = cnums.iter()
                                .map(|&cnum| (cnum, tcx.crate_name(cnum)))
                                .find(|&(_, crate_name)| crate_name == Symbol::intern(&cargo_package_name.replace("-", "_")));

                            if let Some((cnum, _crate_name)) = crate_matching_cargo_package_name {
                                diagnostic.cancel();
                                return cnum;
                            }

                            diagnostic.note("the `CARGO_PKG_NAME` environment variable does not match any of the crate candidates");
                        }

                        for &cnum in cnums {
                            diagnostic.note(format!("crate candidate: `{}`", tcx.crate_name(cnum)));
                        }
                        diagnostic.emit();
                    }
                }
            });

            let all_mutable_fns_count = mutest_emit::analysis::call_graph::all_mutable_fns(tcx, external_meta_mutant_crate, &tests).count();

            let (entry_points, targets, json_definitions) = match &opts.crate_kind {
                config::CrateKind::MutantForExternalTests(external_targets) => {
                    let entry_points = EntryPoints::External;

                    let targets = external_targets.stable_targets.iter()
                        .map(|stable_target| stable_target.into_target_session(tcx, &external_targets.path_strs))
                        .collect::<Vec<_>>();

                    (entry_points, targets, external_targets.json_definitions.clone())
                }
                _ => {
                    let entry_points = EntryPoints::Tests(&tests);

                    let targeting = match external_meta_mutant_crate {
                        None => Targeting::LocalMutables,
                        Some(cnum) => Targeting::ExternMutables(cnum),
                    };

                    let call_graph_depth_limit = opts.call_graph_depth_limit;
                    if let Some(v) = call_graph_depth_limit && v < opts.mutation_depth {
                        tcx.dcx().fatal("mutation depth exceeds explicit call graph depth limit argument");
                    }

                    let call_graph_trace_length_limit = opts.call_graph_trace_length_limit;
                    if let Some(v) = call_graph_trace_length_limit && v < opts.mutation_depth {
                        tcx.dcx().fatal("mutation depth exceeds explicit call graph trace length limit argument");
                    }

                    let t_target_analysis_start = Instant::now();

                    let (call_graph, mut reachable_fns) = mutest_emit::analysis::call_graph::reachable_fns(tcx, &def_res, &generated_crate_ast, entry_points, targeting, call_graph_depth_limit, call_graph_trace_length_limit);
                    let mut json_definitions = Default::default();
                    if let Some(write_opts) = &opts.write_opts {
                        let t_write_start = Instant::now();
                        json_definitions = write_call_graph(write_opts, tcx, all_mutable_fns_count, entry_points, &call_graph, &reachable_fns, t_target_analysis_start.elapsed());
                        pass_result.write_duration += t_write_start.elapsed();
                    }
                    if opts.verbosity >= 1 {
                        println!("built call graph with depth of {depth}",
                            depth = call_graph.depth(),
                        );

                        println!("reached {reached_pct:.2}% of functions from tests ({reached} out of {total} functions)",
                            reached_pct = reachable_fns.len() as f64 / all_mutable_fns_count as f64 * 100_f64,
                            reached = reachable_fns.len(),
                            total = all_mutable_fns_count,
                        );

                        if call_graph.virtual_calls_count >= 1 || call_graph.dynamic_calls_count >= 1 {
                            let total_calls_count = call_graph.total_calls_count();
                            println!("could not resolve {unresolved_pct:.2}% of function calls ({virtual} virtual, {dynamic} dynamic, {foreign} foreign out of {total} function calls)",
                                unresolved_pct = (call_graph.virtual_calls_count + call_graph.dynamic_calls_count + call_graph.foreign_calls_count) as f64 / total_calls_count as f64 * 100_f64,
                                virtual = call_graph.virtual_calls_count,
                                dynamic = call_graph.dynamic_calls_count,
                                foreign = call_graph.foreign_calls_count,
                                total = total_calls_count,
                            );
                        }
                    }

                    // HACK: Ensure that targets are in a deterministic, stable order, otherwise
                    //       mutation IDs will not match between repeated invocations.
                    reachable_fns.sort_unstable_by(|target_a, target_b| {
                        let target_a_span = tcx.def_span(target_a.def_id());
                        let target_b_span = tcx.def_span(target_b.def_id());
                        span_diagnostic_ord(target_a_span, target_b_span)
                    });

                    if let Some(config::CallGraphOptions { format, entry_point_filters, non_local_call_view }) = opts.print_opts.call_graph.take() {
                        if opts.print_opts.print_headers { println!("\n@@@ call graph @@@\n"); }
                        print_call_graph(tcx, entry_points, &call_graph, &reachable_fns, format, &entry_point_filters, non_local_call_view);
                        if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                            if let Some(write_opts) = &opts.write_opts {
                                pass_result.duration = t_start.elapsed();
                                pass_result.target_analysis_duration = t_target_analysis_start.elapsed();
                                write_timings(write_opts, t_start.elapsed(), &pass_result, None, None);
                            }
                            if opts.report_timings {
                                println!("\nfinished in {total:.2?} (targets {targets:.2?}; write {write:.2?})",
                                    total = t_start.elapsed(),
                                    targets = pass_result.test_discovery_duration + t_target_analysis_start.elapsed(),
                                    write = pass_result.write_duration,
                                );
                            }
                            return Flow::Break;
                        }
                        if opts.verbosity >= 1 { println!(); }
                    }

                    let targets = reachable_fns.into_iter()
                        .filter(|f| match f.reachability {
                            TargetReachability::DirectEntry => true,
                            TargetReachability::NestedCallee { distance } => distance < opts.mutation_depth,
                        })
                        .collect::<Vec<_>>();

                    pass_result.target_analysis_duration = t_target_analysis_start.elapsed();

                    if let Some(_) = opts.print_opts.mutation_targets.take() {
                        if opts.print_opts.print_headers { println!("\n@@@ targets @@@\n"); }
                        print_targets(tcx, &opts.crate_kind, &targets, opts.unsafe_targeting);
                        if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                            if let Some(write_opts) = &opts.write_opts {
                                pass_result.duration = t_start.elapsed();
                                write_timings(write_opts, t_start.elapsed(), &pass_result, None, None);
                            }
                            if opts.report_timings {
                                println!("\nfinished in {total:.2?} (targets {targets:.2?}; write {write:.2?})",
                                    total = t_start.elapsed(),
                                    targets = pass_result.test_discovery_duration + pass_result.target_analysis_duration,
                                    write = pass_result.write_duration,
                                );
                            }
                            return Flow::Break;
                        }
                        if opts.verbosity >= 1 { println!(); }
                    }

                    (entry_points, targets, json_definitions)
                }
            };

            if opts.crate_kind.provides_tests() {
                mutest_emit::codegen::expansion::clean_up_test_cases(sess, &tests, &mut generated_crate_ast);
            }

            let body_res = mutest_emit::analysis::ast_lowering::resolve_bodies(tcx, &def_res, &generated_crate_ast);
            if opts.verify_opts.ast_lowering {
                mutest_emit::analysis::ast_lowering::validate_body_resolutions(&body_res, &def_res, &generated_crate_ast);
            }

            if opts.sanitize_macro_expns {
                let t_sanitize_macro_expns_start = Instant::now();
                mutest_emit::codegen::hygiene::sanitize_macro_expansions(tcx, &crate_res, &def_res, &body_res, &mut generated_crate_ast);
                pass_result.sanitize_macro_expns_duration = t_sanitize_macro_expns_start.elapsed();
            }

            if let Some(external_meta_mutant_crate) = external_meta_mutant_crate {
                let Some(rustc_invocation) = crate_const_storage::extract_rustc_invocation(tcx, external_meta_mutant_crate) else {
                    tcx.dcx().fatal("missing rustc invocation metadata in recompilable dependency crate");
                };

                let path_strs = targets.iter()
                    .flat_map(|target| {
                        let EntryPointAssocs::Local(reachable_from) = &target.reachable_from else {
                            bug!("test compiler session generated targets with non-local entry points");
                        };
                        reachable_from.keys().map(|local_entry_point| {
                            let def_path_hash = tcx.def_path_hash(local_entry_point.local_def_id.to_def_id());
                            (def_path_hash, local_entry_point.path_str(tcx))
                        })
                    })
                    .collect::<FxHashMap<_, _>>();

                let crate_name = crate_res.visible_crate_name(external_meta_mutant_crate).as_str().to_owned();
                pass_result.specialized_external_mutant_crate = Some((crate_name, SpecializedMutantCrateCompilationRequest {
                    rustc_invocation,
                    specialized_extra_filename: format!("-for-{}{}", tcx.crate_name(hir::LOCAL_CRATE), sess.opts.cg.extra_filename),
                    external_targets: ExternalTargets {
                        stable_targets: targets.iter().map(|target| StableTarget::from_test_session(tcx, target)).collect::<Vec<_>>(),
                        path_strs,
                        json_definitions,
                    },
                }));

                let t_codegen_start = Instant::now();

                let external_meta_mutant_crate_name = crate_res.visible_crate_name(external_meta_mutant_crate);

                let meta_mutant = MetaMutant::External { crate_name: external_meta_mutant_crate_name };
                perform_codegen(tcx, opts, &mut pass_result, &mut generated_crate_ast, &mut crate_ast, entry_points, meta_mutant);

                pass_result.codegen_duration = t_codegen_start.elapsed();

                pass_result.duration = t_start.elapsed();
                return Flow::Continue(pass_result);
            }

            let t_mutation_generation_start = Instant::now();
            let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, &crate_res, &def_res, &body_res, &generated_crate_ast, &targets, &opts.operators, opts.unsafe_targeting, &sess_opts);
            if opts.verbosity >= 1 {
                let mutated_fns = mutations.iter().map(|m| m.target.def_id()).collect::<FxHashSet<_>>();
                let mutated_fns_count = mutated_fns.len();

                println!("generated {mutations} mutations in {mutated_pct:.2}% of functions ({mutated} out of {total} functions)",
                    mutations = mutations.len(),
                    mutated_pct = mutated_fns_count as f64 / all_mutable_fns_count as f64 * 100_f64,
                    mutated = mutated_fns_count,
                    total = all_mutable_fns_count,
                );
            }
            pass_result.mutation_generation_duration = t_mutation_generation_start.elapsed();

            if let Err(errors) = mutest_emit::codegen::mutation::validate_mutations(&mutations) {
                for error in &errors {
                    use mutest_emit::codegen::mutation::MutationError::*;
                    match error {
                        DummySubsts(mutation, dummy_substs) => {
                            let mut diagnostic = tcx.dcx().struct_err(format!("mutation operator attempted to write {desc}",
                                desc = match dummy_substs.len() {
                                    1 => "dummy substitution".to_owned(),
                                    n => format!("{n} dummy substitutions"),
                                },
                            ));
                            diagnostic.span(mutation.span);
                            diagnostic.span_label(mutation.span, format!("invalid mutation: {}", mutation.mutation.span_label()));
                            diagnostic.emit();
                        }
                    }
                }

                println!("found {} mutation errors", errors.len());
                FatalError.raise();
            }

            let t_mutation_conflict_resolution_start = Instant::now();
            let mutation_conflict_graph = mutest_emit::codegen::mutation::generate_mutation_conflict_graph(&mutations, opts.unsafe_targeting);
            pass_result.mutation_conflict_resolution_duration = t_mutation_conflict_resolution_start.elapsed();

            if opts.verbosity >= 1 {
                println!("found {conflicts} conflicts ({conflicts_excluding_unsafe} excluding unsafe mutations), {compatibilities} compatibilities",
                    conflicts = mutation_conflict_graph.iter_conflicts().count(),
                    conflicts_excluding_unsafe = mutation_conflict_graph.iter_conflicts_excluding_unsafe().count(),
                    compatibilities = mutation_conflict_graph.iter_compatibilities().count(),
                );
            }

            if let Some(config::ConflictGraphOptions { compatibility_graph, exclude_unsafe, format }) = opts.print_opts.conflict_graph.take() {
                if opts.print_opts.print_headers { println!("\n@@@ conflict graph @@@\n"); }
                let mutations_excluding_unsafe = mutations.iter().filter(|m| !mutation_conflict_graph.is_unsafe(m.id));
                match (compatibility_graph, exclude_unsafe) {
                    (false, false) => print_mutation_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_conflicts(), format),
                    (false, true) => print_mutation_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_conflicts_excluding_unsafe(), format),
                    (true, false) => print_mutation_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_compatibilities(), format),
                    (true, true) => print_mutation_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_compatibilities(), format),
                }
                if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                    if let Some(write_opts) = &opts.write_opts {
                        pass_result.duration = t_start.elapsed();
                        write_timings(write_opts, t_start.elapsed(), &pass_result, None, None);
                    }
                    if opts.report_timings {
                        println!("\nfinished in {total:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; conflicts {conflicts:.2?}; write {write:.2?})",
                            total = t_start.elapsed(),
                            targets = pass_result.test_discovery_duration + pass_result.target_analysis_duration,
                            mutations = pass_result.mutation_generation_duration,
                            conflicts = pass_result.mutation_conflict_resolution_duration,
                            write = pass_result.write_duration,
                        );
                    }
                    return Flow::Break;
                }
            }

            let mutation_batches = match &opts.mutation_parallelism {
                None => None,

                Some(config::MutationParallelism::Batching(mutation_batching_opts)) => {
                    let t_mutation_batching_start = Instant::now();
                    let mutation_batches = match mutation_batching_opts.algorithm {
                        config::MutationBatchingAlgorithm::Random => {
                            let mut rng = mutation_batching_opts.randomness.rng();
                            mutest_emit::codegen::mutation::batch_mutations_random(&mutations, &mutation_conflict_graph, mutation_batching_opts.batch_max_mutations_count, &mut rng)
                        }

                        config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, epsilon } => {
                            let mut rng = mutation_batching_opts.randomness.rng();
                            if let Some(v) = epsilon {
                                if v < 0_f64 || v > 1_f64 { panic!("epsilon must be a valid probability"); }
                            }
                            mutest_emit::codegen::mutation::batch_mutations_greedy(
                                &mutations,
                                &mutation_conflict_graph,
                                ordering_heuristic,
                                epsilon,
                                Some(&mut rng),
                                mutation_batching_opts.batch_max_mutations_count,
                            )
                        }

                        config::MutationBatchingAlgorithm::SimulatedAnnealing => {
                            let mut mutation_batches = mutest_emit::codegen::mutation::batch_mutations_dummy(&mutations);

                            let mut rng = mutation_batching_opts.randomness.rng();
                            mutest_emit::codegen::mutation::optimize_batches_simulated_annealing(&mut mutation_batches, &mutation_conflict_graph, mutation_batching_opts.batch_max_mutations_count, 5000, &mut rng);

                            mutation_batches
                        }
                    };
                    pass_result.mutation_batching_duration = t_mutation_batching_start.elapsed();

                    if let Err(errors) = mutest_emit::codegen::mutation::validate_mutation_batches(&mutation_batches, &mutation_conflict_graph) {
                        for error in &errors {
                            use mutest_emit::codegen::mutation::MutationBatchesValidationError::*;
                            match error {
                                ConflictingMutationsInBatch(_mutation_batch, mutations) => {
                                    let mut diagnostic = tcx.dcx().struct_err("batch contains conflicting mutations");
                                    for mutation in mutations {
                                        diagnostic.span_warn(mutation.span, format!("incompatible mutation: {}", mutation.mutation.span_label()));
                                    }
                                    diagnostic.emit();
                                }
                            }
                        }

                        println!("found {} mutation batching errors", errors.len());
                        FatalError.raise();
                    }

                    Some(mutation_batches)
                }
            };

            let mutation_parallelism = match opts.mutation_parallelism {
                None => None,
                Some(config::MutationParallelism::Batching(_)) => {
                    let Some(mutation_batches) = &mutation_batches else { unreachable!() };
                    Some(mutest_emit::codegen::mutation::MutationParallelism::Batched(mutation_batches))
                }
            };

            if let Some(write_opts) = &opts.write_opts {
                let t_write_start = Instant::now();
                write_mutations(write_opts, tcx, all_mutable_fns_count, &json_definitions, &targets, &mutations, opts.unsafe_targeting, &mutation_conflict_graph, mutation_parallelism, t_mutation_generation_start.elapsed());
                pass_result.write_duration += t_write_start.elapsed();
            }

            if let Some(_) = opts.print_opts.mutations.take() {
                if opts.print_opts.print_headers { println!("\n@@@ mutations @@@\n"); }
                print_mutations(tcx, &mutations, mutation_batches.as_deref(), opts.unsafe_targeting, opts.verbosity);
                if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                    if let Some(write_opts) = &opts.write_opts {
                        pass_result.duration = t_start.elapsed();
                        write_timings(write_opts, t_start.elapsed(), &pass_result, None, None);
                    }
                    if opts.report_timings {
                        println!("\nfinished in {total:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; batching {batching:.2?}; write {write:.2?})",
                            total = t_start.elapsed(),
                            targets = pass_result.test_discovery_duration + pass_result.target_analysis_duration,
                            mutations = pass_result.mutation_generation_duration,
                            batching = pass_result.mutation_conflict_resolution_duration + pass_result.mutation_batching_duration,
                            write = pass_result.write_duration,
                        );
                    }
                    return Flow::Break;
                }
            }

            let t_codegen_start = Instant::now();

            let subst_locs = mutest_emit::codegen::substitution::write_substitutions(tcx, &mutations, &mut generated_crate_ast);

            let meta_mutant = MetaMutant::Internal {
                mutations: &mutations,
                subst_locs: &subst_locs,
                mutation_parallelism,
                unsafe_targeting: opts.unsafe_targeting,
            };
            perform_codegen(tcx, opts, &mut pass_result, &mut generated_crate_ast, &mut crate_ast, entry_points, meta_mutant);

            pass_result.codegen_duration = t_codegen_start.elapsed();

            pass_result.duration = t_start.elapsed();
            Flow::Continue(pass_result)
        });

        result.into()
    })?;

    Ok(analysis_pass)
}
