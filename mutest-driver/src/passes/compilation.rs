use std::path::PathBuf;

use rustc_feature::UnstableFeatures;
use rustc_interface::interface::Result as CompilerResult;
use rustc_interface::run_compiler;
use rustc_session::config::{OutputFilenames, Input, OutputTypes, OutputType};
use rustc_session::search_paths::SearchPath;
use rustc_span::{FileName, RealFileName};

use crate::config::Config;
use crate::passes::common_compiler_config;
use crate::passes::analysis::AnalysisPassResult;

pub struct CompilationPassResult {
    pub outputs: OutputFilenames,
}

pub fn run(config: &Config, sysroot: PathBuf, analysis_pass: &AnalysisPassResult) -> CompilerResult<CompilationPassResult> {
    let main_path = config.package_directory_path.join("src/main.rs");

    let mut compiler_config = common_compiler_config(&config, sysroot, Input::Str {
        name: FileName::Real(RealFileName::LocalPath(main_path)),
        input: analysis_pass.generated_crate_code.to_owned(),
    });

    // The generated test cases are behind `#[cfg(test)]`, so the crate must be compiled in test-mode once again.
    compiler_config.opts.test = true;
    // The generated crate code uses many unstable and internal features, most of which are emitted by rustc itself.
    compiler_config.opts.unstable_features = UnstableFeatures::Allow;

    // compiler_config.opts.externs.
    compiler_config.opts.search_paths.push(SearchPath::from_cli_opt("crate=/Users/zalanlevai/Developer/mutest/target/debug", Default::default()));

    let out_path = config.package_directory_path.join("mutest_target");
    compiler_config.opts.output_types = OutputTypes::new(&[
        (OutputType::Exe, Some(out_path.join("mutest_bin"))),
    ]);

    let compilation_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<CompilationPassResult> {
        let (linker, outputs) = compiler.enter(|queries| {
            queries.parse()?;
            queries.expansion()?;
            // let (expanded_crate_ast, resolver, _lint_store) = queries.expansion()?.peek().clone();

            // let tests = mutest_emit::analysis::tests::collect_tests(sess, &expanded_crate_ast);

            // let mut generated_crate_ast = (*expanded_crate_ast).clone();

            // resolver.borrow_mut().access(|resolver| {
            //     let mut ecx = mutest_emit::codegen::expansion::init_ecx(sess, "mutest".to_owned(), resolver, None);

            //     mutest_emit::codegen::tests::remove_dummy_main(&mut generated_crate_ast);
            //     mutest_emit::codegen::harness::generate_harness(&mut ecx, &tests, &mut generated_crate_ast);
            // });

            // println!("{code}",
            //     code = rustc_ast_pretty::pprust::print_crate(
            //         sess.source_map(),
            //         &generated_crate_ast,
            //         FileName::Real(RealFileName::LocalPath(main_path.to_owned())),
            //         "".to_owned(),
            //         &rustc_ast_pretty::pprust::state::NoAnn,
            //         true,
            //         rustc_span::edition::Edition::Edition2021,
            //     ),
            // );

            let outputs = queries.prepare_outputs()?.peek().to_owned();

            queries.global_ctxt()?.peek_mut().enter(|tcx| {
                tcx.analysis(())?;
                Ok(())
            })?;

            queries.ongoing_codegen()?;

            let linker = queries.linker()?;
            Ok((linker, outputs))
        })?;

        linker.link()?;

        Ok(CompilationPassResult {
            outputs,
        })
    })?;

    Ok(compilation_pass)
}
