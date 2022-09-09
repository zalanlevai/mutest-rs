use rustc_interface::run_compiler;
use rustc_interface::interface::Result as CompilerResult;
use rustc_span::edition::Edition;

use crate::config::{self, Config};
use crate::passes::{Flow, base_compiler_config};

pub struct AnalysisPassResult {
    pub generated_crate_code: String,
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

            let mut crate_ast = {
                // NOTE: We must register our custom tool attribute namespace before the relevant attribute validation
                //       is performed during macro expansion. The mutable reference to the AST must be dropped before
                //       any further queries are performed.
                let mut crate_ast = queries.parse()?.peek_mut();
                mutest_emit::codegen::attr::register(&mut crate_ast);
                crate_ast.clone()
            };

            let (expanded_crate_ast, resolver, _lint_store) = queries.expansion()?.peek().clone();

            let tests = mutest_emit::analysis::tests::collect_tests(&expanded_crate_ast);

            // TODO: Generate code based on the original, unexpanded AST instead of the
            //       expanded AST which may contain invalid code that is not equivalent due
            //       to macro hygiene.
            let mut generated_crate_ast = (*expanded_crate_ast).clone();

            queries.global_ctxt()?.peek_mut().enter(|tcx| {
                tcx.analysis(())?;

                resolver.borrow_mut().access(|resolver| {
                    let targets = mutest_emit::codegen::mutation::reachable_fns(tcx, resolver, &generated_crate_ast, &tests, opts.mutation_depth);
                    if let config::Mode::PrintMutationTargets = opts.mode {
                        for target in &targets {
                            println!("tests -({distance})-> {unsafe_marker}{def_path} at {span:#?}",
                                distance = target.distance,
                                unsafe_marker = target.is_unsafe(opts.unsafe_targeting).then_some(format!("[unsafe] ")).unwrap_or_default(),
                                def_path = tcx.def_path_str(target.def_id.to_def_id()),
                                span = tcx.hir().span(tcx.hir().local_def_id_to_hir_id(target.def_id)),
                            );
                            for (&test, &distance) in &target.reachable_from {
                                println!("  ({distance}) {test}",
                                    test = test.path_str(),
                                );
                            }
                        }

                        return Flow::Break;
                    }

                    let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, resolver, &generated_crate_ast, &targets, &opts.operators, opts.unsafe_targeting);
                    let mutants = mutest_emit::codegen::mutation::batch_mutations(mutations, opts.mutant_max_mutations_count, opts.unsafe_targeting);
                    if let config::Mode::PrintMutants = opts.mode {
                        for mutant in &mutants {
                            match mutant.mutations.len() {
                                1 => println!("1 mutation"),
                                _ => println!("{} mutations", mutant.mutations.len()),
                            };

                            for mutation in &mutant.mutations {
                                println!("  - {unsafe_marker}{display_name} in {def_path} at {span:#?}",
                                    unsafe_marker = mutation.is_unsafe(opts.unsafe_targeting).then_some(format!("[unsafe] ")).unwrap_or_default(),
                                    display_name = mutation.display_name(),
                                    def_path = tcx.def_path_str(mutation.target.def_id.to_def_id()),
                                    span = tcx.hir().span(tcx.hir().local_def_id_to_hir_id(mutation.target.def_id)),
                                );
                            }
                        }

                        return Flow::Break;
                    }

                    mutest_emit::codegen::substitution::write_substitutions(resolver, &mutants, &mut generated_crate_ast);

                    // HACK: See below.
                    mutest_emit::codegen::expansion::insert_generated_code_crate_refs(resolver, &mut generated_crate_ast);

                    // Clean up the generated test harness's invalid AST.
                    mutest_emit::codegen::tests::clean_up_test_cases(&tests, &mut generated_crate_ast);
                    mutest_emit::codegen::tests::clean_entry_points(&mut generated_crate_ast);
                    mutest_emit::codegen::tests::generate_dummy_main(resolver, &mut generated_crate_ast);

                    mutest_emit::codegen::harness::generate_harness(sess, resolver, &mutants, &mut generated_crate_ast);

                    mutest_emit::codegen::expansion::module::load_modules(sess, &mut crate_ast);
                    mutest_emit::codegen::expansion::revert_non_local_macro_expansions(&mut generated_crate_ast, &crate_ast);

                    Flow::Continue(())
                })?;

                Flow::Continue(())
            })?;

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
                ),
            );

            Flow::Continue(AnalysisPassResult {
                generated_crate_code,
            })
        });

        result.into()
    })?;

    Ok(analysis_pass)
}
