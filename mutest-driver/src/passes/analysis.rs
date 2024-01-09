use std::time::{Duration, Instant};

use itertools::Itertools;
use mutest_emit::codegen::mutation::{Mut, MutId, Mutant, MutationConflictGraph, Target, UnsafeTargeting, Unsafety};
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

fn print_targets<'tcx>(tcx: TyCtxt<'tcx>, targets: &[Target], unsafe_targeting: UnsafeTargeting) {
    let mut unsafe_targets_count = 0;
    let mut tainted_targets_count = 0;

    for target in targets {
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
            span = tcx.hir().span(tcx.hir().local_def_id_to_hir_id(target.def_id)),
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
        total = targets.len(),
        safe = targets.len() - unsafe_targets_count,
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

    for mutant in mutants {
        total_mutations_count += mutant.mutations.len();

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
                    unsafe_marker = "[tainted] ";
                }
                (true, _) => {
                    unsafe_mutations_count += 1;
                    unsafe_marker = "[unsafe] ";
                }
                (false, _) => {}
            };

            print!("  - ");
            if verbosity >= 1 {
                print!("{}: ", mutation.id.index());
            }
            println!("{unsafe_marker}{display_name} in {def_path} at {span:#?}",
                display_name = mutation.display_name(),
                def_path = tcx.def_path_str(mutation.target.def_id.to_def_id()),
                span = tcx.hir().span(tcx.hir().local_def_id_to_hir_id(mutation.target.def_id)),
            );

            for (&test, entry_point) in &mutation.target.reachable_from {
                println!("    <-({distance})- {tainted_marker}{test}",
                    distance = entry_point.distance,
                    tainted_marker = match mutation.target.is_tainted(test, unsafe_targeting) {
                        true => "[tainted] ",
                        false => "",
                    },
                    test = test.path_str(),
                );
            }
        }

        println!();
    }

    println!("{mutants} mutants; {mutations} mutations; {safe} safe; {unsafe} unsafe ({tainted} tainted)",
        mutants = mutants.len(),
        mutations = total_mutations_count,
        safe = total_mutations_count - unsafe_mutations_count,
        r#unsafe = unsafe_mutations_count,
        tainted = tainted_mutations_count,
    );
}

pub fn run(config: &Config) -> CompilerResult<Option<AnalysisPassResult>> {
    let mut compiler_config = base_compiler_config(config);

    // Compile the crate in test-mode to access tests defined behind `#[cfg(test)]`.
    compiler_config.opts.test = true;

    let opts = &config.opts;
    let source_name = compiler_config.input.source_name();

    let analysis_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<Option<AnalysisPassResult>> {
        let result = compiler.enter(|queries| {
            let sess = compiler.session();

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
                    let (resolver, expanded_crate_ast) = &*tcx.resolver_for_lowering(()).borrow();
                    let def_res = mutest_emit::analysis::ast_lowering::DefResolutions::from_resolver(resolver);

                    // TODO: Generate code based on the original, unexpanded AST instead of the
                    //       expanded AST which may contain invalid code that is not equivalent due
                    //       to macro hygiene.
                    let generated_crate_ast = (**expanded_crate_ast).clone();

                    (generated_crate_ast, def_res)
                };

                let tests = mutest_emit::analysis::tests::collect_tests(&generated_crate_ast);

                tcx.analysis(())?;

                let all_mutable_fns_count = mutest_emit::codegen::mutation::all_mutable_fns(tcx).count();

                let t_target_analysis_start = Instant::now();
                let targets = mutest_emit::codegen::mutation::reachable_fns(tcx, &def_res, &generated_crate_ast, &tests, opts.mutation_depth);
                if opts.verbosity >= 1 {
                    println!("reached {reached_pct:.2}% of functions from tests ({reached} out of {total} functions)",
                        reached_pct = targets.len() as f64 / all_mutable_fns_count as f64 * 100_f64,
                        reached = targets.len(),
                        total = all_mutable_fns_count,
                    );
                }
                target_analysis_duration = t_target_analysis_start.elapsed();

                if let config::Mode::PrintMutationTargets = opts.mode {
                    print_targets(tcx, &targets, opts.unsafe_targeting);
                    if opts.report_timings {
                        println!("\nfinished in {total:.2?} (targets {targets:.2?})",
                            total = t_start.elapsed(),
                            targets = target_analysis_duration,
                        );
                    }
                    return Flow::Break;
                }

                let t_mutation_analysis_start = Instant::now();
                let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, &def_res, &generated_crate_ast, &targets, &opts.operators, opts.unsafe_targeting, opts.verbosity);
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
                                let mut diagnostic = sess.struct_err(format!("mutation operator attempted to write {desc}",
                                    desc = match dummy_substs.len() {
                                        1 => "dummy substitution".to_owned(),
                                        n => format!("{n} dummy substitutions"),
                                    },
                                ));
                                diagnostic.set_span(mutation.span);
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

                if let config::Mode::PrintConflictGraph { compatibility_graph, exclude_unsafe, format } = opts.mode {
                    let mutation_conflict_resolution_duration = t_mutation_batching_start.elapsed();
                    let mutations_excluding_unsafe = mutations.iter().filter(|m| !mutation_conflict_graph.is_unsafe(m.id));
                    match (compatibility_graph, exclude_unsafe) {
                        (false, false) => print_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_conflicts(), format),
                        (false, true) => print_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_conflicts_excluding_unsafe(), format),
                        (true, false) => print_graph(&mutation_conflict_graph, mutations.iter(), mutation_conflict_graph.iter_compatibilities(), format),
                        (true, true) => print_graph(&mutation_conflict_graph, mutations_excluding_unsafe, mutation_conflict_graph.iter_compatibilities(), format),
                    }
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

                let mutants = match opts.mutation_batching_algorithm {
                    config::MutationBatchingAlgorithm::None
                    => mutest_emit::codegen::mutation::batch_mutations_dummy(mutations),

                    config::MutationBatchingAlgorithm::Greedy { ordering_heuristic, #[cfg(feature = "random")] epsilon } => {
                        #[cfg(feature = "random")]
                        let mut rng = opts.mutation_batching_randomness.rng();
                        #[cfg(feature = "random")]
                        let epsilon = epsilon.map(|v| {
                            if v < 0_f64 || v > 1_f64 { panic!("epsilon must be a valid probability"); }
                            let random_choice = opts.mutation_batching_randomness.choice;
                            (v, random_choice)
                        });
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
                        let random_choice = opts.mutation_batching_randomness.choice;
                        mutest_emit::codegen::mutation::batch_mutations_random(mutations, &mutation_conflict_graph, opts.mutant_max_mutations_count, random_choice, &mut rng)
                    }
                };

                mutation_batching_duration = t_mutation_batching_start.elapsed();

                if let Err(errors) = mutest_emit::codegen::mutation::validate_mutation_batches(&mutants, &mutation_conflict_graph) {
                    for error in &errors {
                        use mutest_emit::codegen::mutation::MutationBatchesValidationError::*;
                        match error {
                            ConflictingMutationsInBatch(mutant, mutations) => {
                                let mut diagnostic = sess.struct_err("mutant contains conflicting mutations");
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

                if let config::Mode::PrintMutants = opts.mode {
                    print_mutants(tcx, &mutants, opts.unsafe_targeting, opts.verbosity);
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

                let t_codegen_start = Instant::now();

                mutest_emit::codegen::substitution::write_substitutions(tcx, &mutants, &mut generated_crate_ast);

                // HACK: See below.
                mutest_emit::codegen::expansion::insert_generated_code_crate_refs(tcx, &mut generated_crate_ast);

                mutest_emit::codegen::entry_point::clean_entry_points(sess, &mut generated_crate_ast);
                mutest_emit::codegen::entry_point::generate_dummy_main(tcx, &mut generated_crate_ast);

                mutest_emit::codegen::expansion::load_modules(sess, &mut crate_ast);
                mutest_emit::codegen::expansion::revert_non_local_macro_expansions(&mut generated_crate_ast, &crate_ast);
                mutest_emit::codegen::expansion::clean_up_test_cases(sess, &tests, &mut generated_crate_ast);

                mutest_emit::codegen::harness::generate_harness(tcx, &mutants, &mut generated_crate_ast, opts.unsafe_targeting);

                codegen_duration = t_codegen_start.elapsed();

                // HACK: The generated code is currently based on the expanded AST and contains references to the internals
                //       of macro expansions. These are patched over using a static attribute prelude (here) and a static
                //       set of crate references (above).
                let generated_crate_code = format!("{prelude}\n{code}",
                    prelude = mutest_emit::codegen::expansion::GENERATED_CODE_PRELUDE,
                    code = rustc_ast_pretty::pprust::print_crate(
                        sess.source_map(),
                        &generated_crate_ast,
                        source_name,
                        "".to_owned(),
                        &rustc_ast_pretty::pprust::state::NoAnn,
                        true,
                        Edition::Edition2021,
                        &sess.parse_sess.attr_id_generator,
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
