use std::time::{Duration, Instant};

use mutest_emit::codegen::mutation::{Mutant, MutId, Target, UnsafeTargeting, Unsafety};
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
                let (mut generated_crate_ast, resolutions) = {
                    let (resolver, expanded_crate_ast) = &*tcx.resolver_for_lowering(()).borrow();
                    let resolutions = mutest_emit::analysis::ast_lowering::Resolutions::from_resolver(resolver);

                    // TODO: Generate code based on the original, unexpanded AST instead of the
                    //       expanded AST which may contain invalid code that is not equivalent due
                    //       to macro hygiene.
                    let generated_crate_ast = (**expanded_crate_ast).clone();

                    (generated_crate_ast, resolutions)
                };

                let tests = mutest_emit::analysis::tests::collect_tests(&generated_crate_ast);

                tcx.analysis(())?;

                let t_target_analysis_start = Instant::now();
                let targets = mutest_emit::codegen::mutation::reachable_fns(tcx, &resolutions, &generated_crate_ast, &tests, opts.mutation_depth);
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
                let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, &resolutions, &generated_crate_ast, &targets, &opts.operators, opts.unsafe_targeting);
                if opts.verbosity >= 1 {
                    println!("generated {} mutations", mutations.len());
                }
                mutation_analysis_duration = t_mutation_analysis_start.elapsed();

                let t_mutation_batching_start = Instant::now();

                let mutation_conflict_graph = mutest_emit::codegen::mutation::generate_mutation_conflict_graph(&mutations, opts.unsafe_targeting);
                if opts.verbosity >= 1 {
                    println!("found {conflicts} conflicts, {compatibilities} compatibilities",
                        conflicts = mutation_conflict_graph.iter_conflicts().count(),
                        compatibilities = mutation_conflict_graph.iter_compatibilities().count(),
                    );
                }

                if let config::Mode::PrintConflictGraph { compatibility_graph } = opts.mode {
                    let mutation_conflict_resolution_duration = t_mutation_batching_start.elapsed();
                    fn print_graph<T: Iterator<Item = (MutId, MutId)>>(edge_iter: T) {
                        for (a, b) in edge_iter {
                            println!("{} {}", a.index(), b.index());
                        }
                    }
                    match compatibility_graph {
                        false => print_graph(mutation_conflict_graph.iter_conflicts()),
                        true => print_graph(mutation_conflict_graph.iter_compatibilities()),
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
                    config::MutationBatchingAlgorithm::Greedy
                    => mutest_emit::codegen::mutation::batch_mutations_greedy(mutations, &mutation_conflict_graph, opts.mutant_max_mutations_count),

                    #[cfg(feature = "random")]
                    config::MutationBatchingAlgorithm::Random { seed, attempts } => {
                        use rand::prelude::*;

                        let mut rng = match seed {
                            Some(seed) => StdRng::from_seed(seed),
                            None => StdRng::from_entropy(),
                        };

                        mutest_emit::codegen::mutation::batch_mutations_random(mutations, &mutation_conflict_graph, opts.mutant_max_mutations_count, &mut rng, attempts)
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
                                    diagnostic.span_warn(mutation.location.span(), format!("incompatible mutation: {}", mutation.mutation.span_label()));
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
