use std::time::{Duration, Instant};

use rustc_hash::FxHashSet;
use rustc_interface::run_compiler;
use rustc_interface::interface::Result as CompilerResult;
use rustc_span::edition::Edition;
use rustc_span::fatal_error::FatalError;

use crate::config::{self, Config};
use crate::passes::{Flow, base_compiler_config};
use crate::print::{print_call_graph, print_mutants, print_mutation_graph, print_targets, print_tests};

pub struct AnalysisPassResult {
    pub duration: Duration,
    pub target_analysis_duration: Duration,
    pub sanitize_macro_expns_duration: Duration,
    pub mutation_analysis_duration: Duration,
    pub mutation_batching_duration: Duration,
    pub codegen_duration: Duration,
    pub generated_crate_code: String,
}

pub fn run(config: &mut Config) -> CompilerResult<Option<AnalysisPassResult>> {
    let mut compiler_config = base_compiler_config(config);

    // Compile the crate in test-mode to access tests defined behind `#[cfg(test)]`.
    compiler_config.opts.test = true;

    // NOTE: We must turn off `format_args` optimizations to be able to match up
    //       argument nodes between the AST and the HIR.
    //       See `mutest_emit::analysis::ast_lowering` for more details.
    compiler_config.opts.unstable_opts.flatten_format_args = false;

    let opts = &mut config.opts;
    let source_name = compiler_config.input.source_name();

    let sess_opts = mutest_emit::session::Options {
        verbosity: opts.verbosity,
        report_timings: opts.report_timings,
        sanitize_macro_expns: opts.sanitize_macro_expns,
    };

    let analysis_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<Option<AnalysisPassResult>> {
        let result = compiler.enter(|queries| {
            let sess = &compiler.sess;

            let t_start = Instant::now();
            let mut target_analysis_duration = Duration::ZERO;
            let mut sanitize_macro_expns_duration = Duration::ZERO;
            let mut mutation_analysis_duration = Duration::ZERO;
            let mut mutation_batching_duration = Duration::ZERO;
            let mut codegen_duration = Duration::ZERO;

            let mut crate_ast = {
                // NOTE: We must register our custom tool attribute namespace before the relevant attribute validation
                //       is performed during macro expansion. The mutable reference to the AST must be dropped before
                //       any further queries are performed.
                let mut crate_ast_steal = queries.parse()?;
                let crate_ast = crate_ast_steal.get_mut();
                mutest_emit::codegen::tool_attr::register(sess, crate_ast);
                crate_ast.clone()
            };

            queries.global_ctxt()?.enter(|tcx| {
                let (mut generated_crate_ast, def_res) = {
                    let (resolver, expanded_crate_ast) = &*tcx.resolver_for_lowering().borrow();
                    let def_res = mutest_emit::analysis::ast_lowering::DefResolutions::from_resolver(resolver);

                    // TODO: Generate code based on the original, unexpanded AST instead of the
                    //       expanded AST which may contain invalid code that is not equivalent due
                    //       to macro hygiene.
                    let generated_crate_ast = (**expanded_crate_ast).clone();

                    (generated_crate_ast, def_res)
                };

                let tests = mutest_emit::analysis::tests::collect_tests(&generated_crate_ast, &def_res);

                if let Some(_) = opts.print_opts.tests.take() {
                    if opts.print_opts.print_headers { println!("\n@@@ tests @@@\n"); }
                    print_tests(&tests);
                    if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                        if opts.report_timings {
                            println!("\nfinished in {total:.2?}",
                                total = t_start.elapsed(),
                            );
                        }
                        return Flow::Break;
                    }
                    if opts.verbosity >= 1 { println!(); }
                }

                tcx.analysis(())?;

                let crate_res = mutest_emit::analysis::res::CrateResolutions::from_post_analysis_tcx(tcx);

                let all_mutable_fns_count = mutest_emit::analysis::call_graph::all_mutable_fns(tcx, &tests).count();

                let call_graph_depth = match opts.call_graph_depth {
                    Some(call_graph_depth) => {
                        if call_graph_depth < opts.mutation_depth {
                            tcx.dcx().fatal("explicit call graph depth argument exceeds mutation depth");
                        }
                        call_graph_depth
                    }
                    None => opts.mutation_depth,
                };

                let t_target_analysis_start = Instant::now();

                let (call_graph, mut reachable_fns) = mutest_emit::analysis::call_graph::reachable_fns(tcx, &def_res, &generated_crate_ast, &tests, call_graph_depth);
                if opts.verbosity >= 1 {
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
                reachable_fns.sort_unstable_by_key(|target| tcx.hir().span(tcx.local_def_id_to_hir_id(target.def_id)));

                if let Some(config::CallGraphOptions { format, test_filters, non_local_call_view }) = opts.print_opts.call_graph.take() {
                    if opts.print_opts.print_headers { println!("\n@@@ call graph @@@\n"); }
                    print_call_graph(tcx, &tests, &call_graph, &reachable_fns, format, &test_filters, non_local_call_view);
                    if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                        if opts.report_timings {
                            println!("\nfinished in {total:.2?} (targets {targets:.2?})",
                                total = t_start.elapsed(),
                                targets = t_target_analysis_start.elapsed(),
                            );
                        }
                        return Flow::Break;
                    }
                    if opts.verbosity >= 1 { println!(); }
                }

                let targets = reachable_fns.iter().filter(|f| f.distance < opts.mutation_depth);

                target_analysis_duration = t_target_analysis_start.elapsed();

                if let Some(_) = opts.print_opts.mutation_targets.take() {
                    if opts.print_opts.print_headers { println!("\n@@@ targets @@@\n"); }
                    print_targets(tcx, targets.clone(), opts.unsafe_targeting);
                    if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                        if opts.report_timings {
                            println!("\nfinished in {total:.2?} (targets {targets:.2?})",
                                total = t_start.elapsed(),
                                targets = target_analysis_duration,
                            );
                        }
                        return Flow::Break;
                    }
                    if opts.verbosity >= 1 { println!(); }
                }

                mutest_emit::codegen::expansion::clean_up_test_cases(sess, &tests, &mut generated_crate_ast);

                let body_res = mutest_emit::analysis::ast_lowering::resolve_bodies(tcx, &def_res, &generated_crate_ast);
                if opts.verify_opts.ast_lowering {
                    mutest_emit::analysis::ast_lowering::validate_body_resolutions(&body_res, &def_res, &generated_crate_ast);
                }

                if opts.sanitize_macro_expns {
                    let t_sanitize_macro_expns_start = Instant::now();
                    mutest_emit::codegen::hygiene::sanitize_macro_expansions(tcx, &crate_res, &def_res, &body_res, &mut generated_crate_ast);
                    sanitize_macro_expns_duration = t_sanitize_macro_expns_start.elapsed();
                }

                let t_mutation_analysis_start = Instant::now();
                let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, &crate_res, &def_res, &body_res, &generated_crate_ast, targets, &opts.operators, opts.unsafe_targeting, &sess_opts);
                if opts.verbosity >= 1 {
                    let mutated_fns = mutations.iter().map(|m| m.target.def_id).collect::<FxHashSet<_>>();
                    let mutated_fns_count = mutated_fns.len();

                    println!("generated {mutations} mutations in {mutated_pct:.2}% of functions ({mutated} out of {total} functions)",
                        mutations = mutations.len(),
                        mutated_pct = mutated_fns_count as f64 / all_mutable_fns_count as f64 * 100_f64,
                        mutated = mutated_fns_count,
                        total = all_mutable_fns_count,
                    );
                }
                mutation_analysis_duration = t_mutation_analysis_start.elapsed();

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

                let t_mutation_batching_start = Instant::now();

                let mutation_conflict_graph = mutest_emit::codegen::mutation::generate_mutation_conflict_graph(&mutations, opts.unsafe_targeting);
                if opts.verbosity >= 1 {
                    println!("found {conflicts} conflicts ({conflicts_excluding_unsafe} excluding unsafe mutations), {compatibilities} compatibilities",
                        conflicts = mutation_conflict_graph.iter_conflicts().count(),
                        conflicts_excluding_unsafe = mutation_conflict_graph.iter_conflicts_excluding_unsafe().count(),
                        compatibilities = mutation_conflict_graph.iter_compatibilities().count(),
                    );
                }

                if let Some(config::ConflictGraphOptions { compatibility_graph, exclude_unsafe, format }) = opts.print_opts.conflict_graph.take() {
                    let mutation_conflict_resolution_duration = t_mutation_batching_start.elapsed();
                    if opts.print_opts.print_headers { println!("\n@@@ conflict graph @@@\n"); }
                    let mutations_excluding_unsafe = mutations.iter().filter(|m| !mutation_conflict_graph.is_unsafe(m.id));
                    match (compatibility_graph, exclude_unsafe) {
                        (false, false) => print_mutation_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_conflicts(), format),
                        (false, true) => print_mutation_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_conflicts_excluding_unsafe(), format),
                        (true, false) => print_mutation_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_compatibilities(), format),
                        (true, true) => print_mutation_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_compatibilities(), format),
                    }
                    if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                        if opts.report_timings {
                            println!("\nfinished in {total:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; conflicts {conflicts:.2?})",
                                total = t_start.elapsed(),
                                targets = target_analysis_duration,
                                mutations = mutation_analysis_duration,
                                conflicts = mutation_conflict_resolution_duration,
                            );
                        }
                        return Flow::Break;
                    }
                }

                let mutants = match opts.mutation_batching_algorithm {
                    config::MutationBatchingAlgorithm::None
                    => mutest_emit::codegen::mutation::batch_mutations_dummy(mutations),

                    config::MutationBatchingAlgorithm::Random => {
                        let mut rng = opts.mutation_batching_randomness.rng();
                        mutest_emit::codegen::mutation::batch_mutations_random(mutations, &mutation_conflict_graph, opts.mutant_max_mutations_count, &mut rng)
                    }

                    config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, epsilon } => {
                        let mut rng = opts.mutation_batching_randomness.rng();
                        if let Some(v) = epsilon {
                            if v < 0_f64 || v > 1_f64 { panic!("epsilon must be a valid probability"); }
                        }
                        mutest_emit::codegen::mutation::batch_mutations_greedy(
                            mutations,
                            &mutation_conflict_graph,
                            ordering_heuristic,
                            epsilon,
                            Some(&mut rng),
                            opts.mutant_max_mutations_count,
                        )
                    }

                    config::MutationBatchingAlgorithm::SimulatedAnnealing => {
                        let mut mutants = mutest_emit::codegen::mutation::batch_mutations_dummy(mutations);

                        let mut rng = opts.mutation_batching_randomness.rng();
                        mutest_emit::codegen::mutation::optimize_batches_simulated_annealing(&mut mutants, &mutation_conflict_graph, opts.mutant_max_mutations_count, 5000, &mut rng);

                        mutants
                    }
                };

                mutation_batching_duration = t_mutation_batching_start.elapsed();

                if let Err(errors) = mutest_emit::codegen::mutation::validate_mutation_batches(&mutants, &mutation_conflict_graph) {
                    for error in &errors {
                        use mutest_emit::codegen::mutation::MutationBatchesValidationError::*;
                        match error {
                            ConflictingMutationsInBatch(_mutant, mutations) => {
                                let mut diagnostic = tcx.dcx().struct_err("mutant contains conflicting mutations");
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

                if let Some(_) = opts.print_opts.mutants.take() {
                    if opts.print_opts.print_headers { println!("\n@@@ mutants @@@\n"); }
                    print_mutants(tcx, &mutants, opts.unsafe_targeting, opts.verbosity);
                    if let config::Mode::Print = opts.mode && opts.print_opts.is_empty() {
                        if opts.report_timings {
                            println!("\nfinished in {total:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; batching {batching:.2?})",
                                total = t_start.elapsed(),
                                targets = target_analysis_duration,
                                mutations = mutation_analysis_duration,
                                batching = mutation_batching_duration,
                            );
                        }
                        return Flow::Break;
                    }
                }

                let t_codegen_start = Instant::now();

                let subst_locs = mutest_emit::codegen::substitution::write_substitutions(tcx, &mutants, &mut generated_crate_ast);

                // HACK: See below.
                mutest_emit::codegen::expansion::insert_generated_code_crate_refs(tcx, &mut generated_crate_ast);
                mutest_emit::codegen::expansion::insert_generated_code_prelude_attrs(tcx, &mut generated_crate_ast);

                mutest_emit::codegen::entry_point::clean_entry_points(sess, &mut generated_crate_ast);
                mutest_emit::codegen::entry_point::generate_dummy_main(tcx, &mut generated_crate_ast);

                if !opts.sanitize_macro_expns {
                    mutest_emit::codegen::expansion::load_modules(sess, &mut crate_ast);
                    mutest_emit::codegen::expansion::revert_non_local_macro_expansions(&mut generated_crate_ast, &crate_ast);
                }

                mutest_emit::codegen::substitution::resolve_syntax_ambiguities(tcx, &mut generated_crate_ast);

                mutest_emit::codegen::harness::generate_harness(tcx, &mutants, &subst_locs, &mut generated_crate_ast, opts.unsafe_targeting);

                codegen_duration = t_codegen_start.elapsed();

                // HACK: The generated code is currently based on the expanded AST and contains references to the internals
                //       of macro expansions. These are patched over using a static attribute prelude (here) and a static
                //       set of crate references (above).
                struct NoAnn;
                impl rustc_ast_pretty::pprust::state::PpAnn for NoAnn {}
                let generated_crate_code = format!("{prelude}\n{code}",
                    prelude = mutest_emit::codegen::expansion::GENERATED_CODE_PRELUDE,
                    code = rustc_ast_pretty::pprust::print_crate(
                        sess.source_map(),
                        &generated_crate_ast,
                        source_name,
                        "".to_owned(),
                        &NoAnn,
                        true,
                        Edition::Edition2021,
                        &sess.psess.attr_id_generator,
                    ),
                );

                Flow::Continue(AnalysisPassResult {
                    duration: t_start.elapsed(),
                    target_analysis_duration,
                    sanitize_macro_expns_duration,
                    mutation_analysis_duration,
                    mutation_batching_duration,
                    codegen_duration,
                    generated_crate_code,
                })
            })
        });

        result.into()
    })?;

    Ok(analysis_pass)
}
