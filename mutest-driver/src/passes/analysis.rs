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

    let mut compiler_config = common_compiler_config(&config, sysroot, Input::File(main_path.to_owned()));

    // Compile the crate in test-mode for the proper macro expansion behavior.
    compiler_config.opts.test = true;

    let analysis_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<AnalysisPassResult> {
        let result = compiler.enter(|queries| {
            let sess = compiler.session();

            let crate_ast = queries.parse()?.peek().clone();
            let (expanded_crate_ast, resolver, _lint_store) = queries.expansion()?.peek().clone();

            let tests = mutest_emit::analysis::tests::collect_tests(sess, &expanded_crate_ast);
            println!("TESTS:");
            for test in &tests {
                println!("  - {} ({}) -> {} ({})", test.descriptor.ident, test.descriptor.id, test.item.ident, test.item.id);
            }

            let mut generated_crate_ast = (*expanded_crate_ast).clone();
            // let mut generated_crate_ast = crate_ast.clone();

            resolver.borrow_mut().access(|resolver| {
                let mut ecx = mutest_emit::codegen::expansion::init_ecx(sess, "mutest".to_owned(), resolver, None);

                // resolver.resolve_ident_in_module_unadjusted(module, ident, ns, parent_scope, record_used, path_span)

                let ops: mutest_emit::codegen::mutation::Operators = vec![
                    &mutest_mutators::ReplaceArgWithDefault,
                ];
                let mutations = mutest_emit::codegen::mutation::apply_mutation_operators(&mut ecx, ops, &generated_crate_ast);
                let mutants = mutest_emit::codegen::mutation::batch_mutations(mutations);
                mutest_emit::codegen::substitution::write_substitutions(&mut ecx, &mutants, &mut generated_crate_ast);

                mutest_emit::codegen::tests::clean_up_test_cases(&mut ecx, &tests, &mut generated_crate_ast);
                mutest_emit::codegen::tests::clean_entry_points(sess, &mut generated_crate_ast);
                mutest_emit::codegen::tests::generate_dummy_main(&mut ecx, &mut generated_crate_ast);
                // mutest_emit::codegen::harness::generate_harness(&mut ecx, &mut generated_crate_ast);
                mutest_emit::codegen::harness::generate_harness(&mut ecx, &mutants, &mut generated_crate_ast);

                mutest_emit::experiments::test_mutator::prepend_message_to_tests(&mut ecx, &tests, &mut generated_crate_ast);
            });

            queries.global_ctxt()?.peek_mut().enter(|tcx| {
                // tcx.analysis(())?;
                // tcx.hir().visit_all_item_likes(&mut HirVisitor { tcx }.as_deep_visitor());

                // tcx.hir().
                // tcx.

                // let mut acx = AnnotationCtxt::new(&crate_ast);
                // // AstAnnotationWriter { tcx }.visit_crate(&mut acx.annotated_ast);
                // let annotated_ast = acx.annotate(tcx);

                // println!("{:#?}", acx.annotated_ast);
                // println!("{:#?}", annotated_ast);

                // println!("{}", rustc_ast_pretty::pprust::print_crate(
                //     tcx.sess.source_map(),
                //     // &acx.annotated_ast,
                //     &annotated_ast,
                //     // FileName::Real(RealFileName::Remapped {
                //     //     local_path: Some(config.package_directory_path.join("src/main.rs")),
                //     //     virtual_name: config.package_directory_path.join("src/main.rs"),
                //     // }),
                //     FileName::Real(RealFileName::LocalPath(config.package_directory_path.join("src/main.rs"))),
                //     "".to_owned(),
                //     &rustc_ast_pretty::pprust::state::NoAnn,
                //     false,
                //     rustc_span::edition::Edition::Edition2021,
                // ));

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
