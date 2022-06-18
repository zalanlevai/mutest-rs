use rustc_feature::UnstableFeatures;
use rustc_interface::interface::Result as CompilerResult;
use rustc_interface::run_compiler;
use rustc_lint_defs::Level as LintLevel;
use rustc_session::config::{Input, OutputFilenames};
use rustc_session::search_paths::SearchPath;

use crate::config::Config;
use crate::passes::base_compiler_config;
use crate::passes::analysis::AnalysisPassResult;

pub struct CompilationPassResult {
    pub outputs: OutputFilenames,
}

pub fn run(config: &Config, analysis_pass: &AnalysisPassResult) -> CompilerResult<CompilationPassResult> {
    let mut compiler_config = base_compiler_config(config);
    compiler_config.input = Input::Str {
        name: compiler_config.input.source_name(),
        input: analysis_pass.generated_crate_code.to_owned(),
    };

    // The generated crate code relies on the rustc test harness using a custom test runner.
    compiler_config.opts.test = true;
    // The generated crate code uses many unstable and internal features, most of which are emitted by rustc itself.
    compiler_config.opts.unstable_features = UnstableFeatures::Allow;
    // Disable lints on generated crate code.
    compiler_config.opts.lint_cap = Some(LintLevel::Allow);

    // The generated crate code relies on the `mutest_runtime` crate which must be loaded.
    // FIXME: Currently, an invocation of the tool from the mutest project's
    //        workspace root is assumed. We will need to add more flexibility to
    //        the resolution of this injected dependency, potentially by specifying
    //        it via an argument to mutest, and the `compiler_config.opts.externs`
    //        option.
    let mutest_search_path = format!("{}/target/debug", std::env::current_dir().unwrap().display());
    compiler_config.opts.search_paths.push(SearchPath::from_cli_opt(&format!("crate={mutest_search_path}"), Default::default()));

    let compilation_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<CompilationPassResult> {
        let (linker, outputs) = compiler.enter(|queries| {
            queries.parse()?;
            queries.expansion()?;

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
