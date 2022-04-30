use std::path::PathBuf;

use rustc_interface::run_compiler;
use rustc_interface::interface::Result as CompilerResult;
use rustc_session::config::Input;
use rustc_span::{FileName, RealFileName};
use rustc_span::edition::Edition;

use crate::config::{self, Config};
use crate::passes::{Flow, common_compiler_config};

pub struct AnalysisPassResult {
    pub generated_crate_code: String,
}

pub fn run(config: &Config, sysroot: PathBuf) -> CompilerResult<Option<AnalysisPassResult>> {
    let crate_root_path = config.crate_root_path();

    let mut compiler_config = common_compiler_config(config, sysroot, Input::File(crate_root_path.to_owned()));

    // Compile the crate in test-mode to access tests defined behind `#[cfg(test)]`.
    compiler_config.opts.test = true;

    let analysis_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<Option<AnalysisPassResult>> {
        let result = compiler.enter(|queries| {
            let sess = compiler.session();

            let _crate_ast = queries.parse()?.peek().clone();
            let (expanded_crate_ast, resolver, _lint_store) = queries.expansion()?.peek().clone();

            let tests = mutest_emit::analysis::tests::collect_tests(&expanded_crate_ast);

            // TODO: Generate code based on the original, unexpanded AST instead of the
            //       expanded AST which may contain invalid code that is not equivalent due
            //       to macro hygiene.
            let mut generated_crate_ast = (*expanded_crate_ast).clone();

            queries.global_ctxt()?.peek_mut().enter(|tcx| {
                tcx.analysis(())?;

                resolver.borrow_mut().access(|resolver| {
                    let targets = mutest_emit::codegen::mutation::reachable_fns(tcx, resolver, &generated_crate_ast, &tests, config.opts.mutation_depth);
                    if let config::Mode::PrintMutationTargets = config.opts.mode {
                        for target in &targets {
                            println!("tests -({distance})-> {ident:#?} @ {span:#?}",
                                distance = target.distance,
                                ident = target.item.ident(),
                                span = target.item.span(),
                            );
                            for &(test, distance) in &target.reachable_from {
                                println!("  ({distance}) {test}",
                                    test = test.path_str(),
                                );
                            }
                        }

                        return Flow::Break;
                    }

                    let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(tcx, resolver, &targets, &config.opts.operators);
                    let mutants = mutest_emit::codegen::mutation::batch_mutations(&targets, mutations, config.opts.mutant_max_mutations_count);
                    mutest_emit::codegen::substitution::write_substitutions(resolver, &mutants, &mut generated_crate_ast);

                    // Clean up the generated test harness's invalid AST.
                    mutest_emit::codegen::tests::clean_up_test_cases(&tests, &mut generated_crate_ast);
                    mutest_emit::codegen::tests::clean_entry_points(&mut generated_crate_ast);
                    mutest_emit::codegen::tests::generate_dummy_main(resolver, &mut generated_crate_ast);

                    mutest_emit::codegen::harness::generate_harness(sess, resolver, &mutants, &mut generated_crate_ast);

                    Flow::Continue(())
                })?;

                Flow::Continue(())
            })?;

            // HACK: The generated code is currently based on the expanded AST and contains
            //       references to the internals of macro expansions. These are patched over using a
            //       static attribute prelude and a static set of crate references.
            let generated_crate_code = format!("{prelude}\n{code}\n{crate_refs}",
                prelude = mutest_emit::codegen::expansion::GENERATED_CODE_PRELUDE,
                crate_refs = mutest_emit::codegen::expansion::GENERATED_CODE_CRATE_REFS,
                code = rustc_ast_pretty::pprust::print_crate(
                    sess.source_map(),
                    &generated_crate_ast,
                    FileName::Real(RealFileName::LocalPath(crate_root_path.to_owned())),
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
