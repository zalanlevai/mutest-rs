use std::time::{Duration, Instant};

use itertools::Itertools;
use mutest_emit::codegen::mutation::{Mut, MutId, Mutant, MutationConflictGraph, Target, UnsafeTargeting, Unsafety};
use rustc_hash::FxHashMap;
use rustc_interface::run_compiler;
use rustc_interface::interface::Result as CompilerResult;
use rustc_middle::ty::TyCtxt;
use rustc_span::edition::Edition;
use rustc_span::fatal_error::FatalError;

use crate::config::{self, Config};
use crate::passes::{Flow, base_compiler_config};

pub struct AnalysisPassResult {
    pub duration: Duration,
    pub target_analysis_duration: Duration,
    pub mutation_analysis_duration: Duration,
    pub mutation_batching_duration: Duration,
    pub codegen_duration: Duration,
    pub generated_crate_code: String,
}

fn print_targets<'tcx, 'trg>(tcx: TyCtxt<'tcx>, targets: impl Iterator<Item = &'trg Target<'trg>>, unsafe_targeting: UnsafeTargeting) {
    let mut unsafe_targets_count = 0;
    let mut tainted_targets_count = 0;

    let mut targets_count = 0;

    for target in targets {
        targets_count += 1;

        let mut unsafe_marker = "";
        match (target.unsafety.is_unsafe(unsafe_targeting), target.unsafety) {
            (true, Unsafety::Tainted(_)) => {
                unsafe_targets_count += 1;
                tainted_targets_count += 1;
                unsafe_marker = "[tainted] ";
            }
            (true, _) => {
                unsafe_targets_count += 1;
                unsafe_marker = "[unsafe] ";
            }
            (false, _) => {}
        };

        println!("tests -({distance})-> {unsafe_marker}{def_path} at {span:#?}",
            distance = target.distance,
            def_path = tcx.def_path_str(target.def_id.to_def_id()),
            span = tcx.hir().span(tcx.local_def_id_to_hir_id(target.def_id)),
        );

        for (&test, entry_point) in &target.reachable_from {
            println!("  ({distance}) {tainted_marker}{test}",
                distance = entry_point.distance,
                tainted_marker = match target.is_tainted(test, unsafe_targeting) {
                    true => "[tainted] ",
                    false => "",
                },
                test = test.path_str(),
            );
        }

        println!();
    }

    println!("targets: {total} total; {safe} safe; {unsafe} unsafe ({tainted} tainted)",
        total = targets_count,
        safe = targets_count - unsafe_targets_count,
        r#unsafe = unsafe_targets_count,
        tainted = tainted_targets_count,
    );
}

fn print_graph<'trg: 'op, 'm: 'op, 'op, N, E>(mutation_conflict_graph: &MutationConflictGraph<'m>, mutations_iter: N, edge_iter: E, format: config::GraphFormat)
where
    N: IntoIterator<Item = &'op Mut<'trg, 'm>>,
    E: IntoIterator<Item = (MutId, MutId)>,
{
    match format {
        config::GraphFormat::Simple => {
            for (a, b) in edge_iter {
                println!("{} {}", a.index(), b.index());
            }
        }
        config::GraphFormat::Graphviz => {
            println!("strict graph {{");
            println!("  overlap=\"false\";");
            println!("  splines=\"true\";");

            for m in mutations_iter.into_iter() {
                let mut attrs = vec![];
                if mutation_conflict_graph.is_unsafe(m.id) {
                    attrs.push("color=\"red\"");
                    attrs.push("fontcolor=\"red\"");
                }

                println!("  {}{attrs};", m.id.index(),
                    attrs = match attrs.is_empty() {
                        true => "".to_owned(),
                        false => format!(" [{}]", attrs.join(", ")),
                    },
                );
            }

            for (a, b) in edge_iter.into_iter() {
                println!("  {} -- {};", a.index(), b.index());
            }

            println!("}}");
        }
    }
}

fn print_mutants<'tcx>(tcx: TyCtxt<'tcx>, mutants: &[Mutant], unsafe_targeting: UnsafeTargeting, verbosity: u8) {
    let mut total_mutations_count = 0;
    let mut unsafe_mutations_count = 0;
    let mut tainted_mutations_count = 0;
    let mut unbatched_mutations_count = 0;

    for mutant in mutants {
        total_mutations_count += mutant.mutations.len();
        if mutant.mutations.len() == 1 {
            unbatched_mutations_count += 1;
        }

        if verbosity >= 1 {
            print!("{}: ", mutant.id.index());
        }
        match mutant.mutations.len() {
            1 => println!("1 mutation"),
            _ => println!("{} mutations", mutant.mutations.len()),
        };

        for mutation in &mutant.mutations {
            let mut unsafe_marker = "";
            match (mutation.is_unsafe(unsafe_targeting), mutation.target.unsafety) {
                (true, Unsafety::Tainted(_)) => {
                    unsafe_mutations_count += 1;
                    tainted_mutations_count += 1;
                    unsafe_marker = "(tainted) ";
                }
                (true, _) => {
                    unsafe_mutations_count += 1;
                    unsafe_marker = "(unsafe) ";
                }
                (false, _) => {}
            };

            print!("  - ");
            if verbosity >= 1 {
                print!("{}: ", mutation.id.index());
            }
            println!("{unsafe_marker}[{op_name}] {display_name} in {def_path} at {display_location}",
                op_name = mutation.op_name(),
                display_name = mutation.display_name(),
                def_path = tcx.def_path_str(mutation.target.def_id.to_def_id()),
                display_location = mutation.display_location(tcx.sess),
            );

            for (&test, entry_point) in &mutation.target.reachable_from {
                println!("    <-({distance})- {tainted_marker}{test}",
                    distance = entry_point.distance,
                    tainted_marker = match mutation.target.is_tainted(test, unsafe_targeting) {
                        true => "(tainted) ",
                        false => "",
                    },
                    test = test.path_str(),
                );
            }
        }

        println!();
    }

    if verbosity >= 1 {
        #[derive(Clone, Copy, Default)]
        struct MutationOpStats {
            total_mutations_count: usize,
            unsafe_mutations_count: usize,
            tainted_mutations_count: usize,
            unbatched_mutations_count: usize,
        }

        let mut mutation_op_stats: FxHashMap<&str, MutationOpStats> = Default::default();
        for mutant in mutants {
            for mutation in &mutant.mutations {
                let op_stats = mutation_op_stats.entry(mutation.op_name()).or_default();
                op_stats.total_mutations_count += 1;
                if mutation.is_unsafe(unsafe_targeting) { op_stats.unsafe_mutations_count += 1; }
                if let Unsafety::Tainted(_) = mutation.target.unsafety { op_stats.tainted_mutations_count +=1; }
                if mutant.mutations.len() == 1 { op_stats.unbatched_mutations_count += 1; }
            }
        }

        let op_name_w = mutest_operators::ALL.iter().map(|s| s.len()).max().unwrap_or(0);
        let mutations_w = mutation_op_stats.values().map(|s| s.total_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let safe_w = mutation_op_stats.values().map(|s| (s.total_mutations_count - s.unsafe_mutations_count).checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let unsafe_w = mutation_op_stats.values().map(|s| s.unsafe_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let tainted_w = mutation_op_stats.values().map(|s| s.tainted_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let batched_w = mutation_op_stats.values().map(|s| (s.total_mutations_count - s.unbatched_mutations_count).checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let unbatched_w = mutation_op_stats.values().map(|s| s.unbatched_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);

        // TODO: Only list statistics for active mutation operators.
        for op_name in mutest_operators::ALL {
            let op_stats = mutation_op_stats.get(op_name).map(|s| *s).unwrap_or_default();

            println!("{op_name:>op_name_w$}: {mutations_pct:>6}. {mutations:>mutations_w$} mutations; {safe:>safe_w$} safe; {unsafe:>unsafe_w$} unsafe ({tainted:>tainted_w$} tainted); {batched:>batched_w$} batched; {unbatched:>unbatched_w$} unbatched",
                mutations_pct = format!("{:.2}%", op_stats.total_mutations_count as f64 / total_mutations_count as f64 * 100_f64),
                mutations = op_stats.total_mutations_count,
                safe = op_stats.total_mutations_count - op_stats.unsafe_mutations_count,
                r#unsafe = op_stats.unsafe_mutations_count,
                tainted = op_stats.tainted_mutations_count,
                batched = op_stats.total_mutations_count - op_stats.unbatched_mutations_count,
                unbatched = op_stats.unbatched_mutations_count,
            );
        }

        println!();
    }

    println!("{mutants} mutants; {mutations} mutations; {safe} safe; {unsafe} unsafe ({tainted} tainted); {batched} batched; {unbatched} unbatched",
        mutants = mutants.len(),
        mutations = total_mutations_count,
        safe = total_mutations_count - unsafe_mutations_count,
        r#unsafe = unsafe_mutations_count,
        tainted = tainted_mutations_count,
        batched = total_mutations_count - unbatched_mutations_count,
        unbatched = unbatched_mutations_count,
    );
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

                tcx.analysis(())?;

                let all_mutable_fns_count = mutest_emit::codegen::mutation::all_mutable_fns(tcx, &tests).count();

                let t_target_analysis_start = Instant::now();
                let reachable_fns = mutest_emit::codegen::mutation::reachable_fns(tcx, &def_res, &generated_crate_ast, &tests, opts.call_graph_depth);
                if opts.verbosity >= 1 {
                    println!("reached {reached_pct:.2}% of functions from tests ({reached} out of {total} functions)",
                        reached_pct = reachable_fns.len() as f64 / all_mutable_fns_count as f64 * 100_f64,
                        reached = reachable_fns.len(),
                        total = all_mutable_fns_count,
                    );
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

                if opts.sanitize_macro_expns {
                    mutest_emit::codegen::hygiene::sanitize_macro_expansions(tcx, &def_res, &mut generated_crate_ast);
                }

                let t_mutation_analysis_start = Instant::now();
                let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, &def_res, &generated_crate_ast, targets, &opts.operators, opts.unsafe_targeting, &sess_opts);
                if opts.verbosity >= 1 {
                    let mutated_fns = mutations.iter().map(|m| m.target.def_id).unique();
                    let mutated_fns_count = mutated_fns.count();

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
                        (false, false) => print_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_conflicts(), format),
                        (false, true) => print_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_conflicts_excluding_unsafe(), format),
                        (true, false) => print_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_compatibilities(), format),
                        (true, true) => print_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_compatibilities(), format),
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

                    config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, #[cfg(feature = "random")] epsilon } => {
                        #[cfg(feature = "random")]
                        let mut rng = opts.mutation_batching_randomness.rng();
                        #[cfg(feature = "random")]
                        if let Some(v) = epsilon {
                            if v < 0_f64 || v > 1_f64 { panic!("epsilon must be a valid probability"); }
                        }
                        mutest_emit::codegen::mutation::batch_mutations_greedy(
                            mutations,
                            &mutation_conflict_graph,
                            ordering_heuristic,
                            #[cfg(feature = "random")] epsilon,
                            #[cfg(feature = "random")] Some(&mut rng),
                            opts.mutant_max_mutations_count,
                        )
                    }

                    #[cfg(feature = "random")]
                    config::MutationBatchingAlgorithm::Random => {
                        let mut rng = opts.mutation_batching_randomness.rng();
                        mutest_emit::codegen::mutation::batch_mutations_random(mutations, &mutation_conflict_graph, opts.mutant_max_mutations_count, &mut rng)
                    }

                    #[cfg(feature = "random")]
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
                            ConflictingMutationsInBatch(mutant, mutations) => {
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

                mutest_emit::codegen::substitution::write_substitutions(tcx, &mutants, &mut generated_crate_ast);

                // HACK: See below.
                mutest_emit::codegen::expansion::insert_generated_code_crate_refs(tcx, &mut generated_crate_ast);

                mutest_emit::codegen::entry_point::clean_entry_points(sess, &mut generated_crate_ast);
                mutest_emit::codegen::entry_point::generate_dummy_main(tcx, &mut generated_crate_ast);

                if !opts.sanitize_macro_expns {
                    mutest_emit::codegen::expansion::load_modules(sess, &mut crate_ast);
                    mutest_emit::codegen::expansion::revert_non_local_macro_expansions(&mut generated_crate_ast, &crate_ast);
                }

                mutest_emit::codegen::substitution::resolve_syntax_ambiguities(tcx, &mut generated_crate_ast);

                mutest_emit::codegen::harness::generate_harness(tcx, &mutants, &mut generated_crate_ast, opts.unsafe_targeting);

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
