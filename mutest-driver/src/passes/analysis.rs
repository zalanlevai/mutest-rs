use std::path::PathBuf;

use rustc_interface::run_compiler;
use rustc_interface::interface::Result as CompilerResult;
use rustc_session::config::Input;
use rustc_span::{FileName, RealFileName};
use rustc_span::edition::Edition;

use crate::config::Config;
use crate::passes::common_compiler_config;

pub struct AnalysisPassResult {
    pub generated_crate_code: String,
}

pub fn run(config: &Config, sysroot: PathBuf) -> CompilerResult<AnalysisPassResult> {
    let main_path = config.package_directory_path.join("src/main.rs");

    let mut compiler_config = common_compiler_config(config, sysroot, Input::File(main_path.to_owned()));

    // Compile the crate in test-mode to access tests defined behind `#[cfg(test)]`.
    compiler_config.opts.test = true;

    let analysis_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<AnalysisPassResult> {
        let result = compiler.enter(|queries| {
            let sess = compiler.session();

            let _crate_ast = queries.parse()?.peek().clone();
            let (expanded_crate_ast, resolver, _lint_store) = queries.expansion()?.peek().clone();

            let tests = mutest_emit::analysis::tests::collect_tests(&expanded_crate_ast);

            // TODO: Generate code based on the original, unexpanded AST instead of the
            //       expanded AST which may contain invalid code that is not equivalent due
            //       to macro hygiene.
            let mut generated_crate_ast = (*expanded_crate_ast).clone();

            resolver.borrow_mut().access(|resolver| {
                let mut ecx = mutest_emit::codegen::expansion::init_ecx(sess, "mutest".to_owned(), resolver, None);

                let ops: mutest_emit::codegen::mutation::Operators = vec![];
                let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(&mut ecx, ops, &generated_crate_ast);
                let mutants = mutest_emit::codegen::mutation::batch_mutations(mutations);
                mutest_emit::codegen::substitution::write_substitutions(&mut ecx, &mutants, &mut generated_crate_ast);

                // Clean up the generated test harness's invalid AST.
                mutest_emit::codegen::tests::clean_up_test_cases(&tests, &mut generated_crate_ast);
                mutest_emit::codegen::tests::clean_entry_points(&mut generated_crate_ast);
                mutest_emit::codegen::tests::generate_dummy_main(&mut ecx, &mut generated_crate_ast);

                mutest_emit::codegen::harness::generate_harness(&mut ecx, &mutants, &mut generated_crate_ast);
            });

            queries.global_ctxt()?.peek_mut().enter(|tcx| {
                tcx.analysis(())?;
                Ok(())
            })?;

            let generated_crate_code = format!("{prelude}\n{code}",
                prelude = mutest_emit::codegen::expansion::GENERATED_CODE_PRELUDE,
                code = rustc_ast_pretty::pprust::print_crate(
                    sess.source_map(),
                    &generated_crate_ast,
                    FileName::Real(RealFileName::LocalPath(main_path.to_owned())),
                    "".to_owned(),
                    &rustc_ast_pretty::pprust::state::NoAnn,
                    true,
                    Edition::Edition2021,
                ),
            );

            Ok(AnalysisPassResult {
                generated_crate_code,
            })
        })?;

        Ok(result)
    })?;

    Ok(analysis_pass)
}
