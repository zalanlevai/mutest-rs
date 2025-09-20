use std::sync::Arc;
use std::time::{Duration, Instant};

use rustc_feature::UnstableFeatures;
use rustc_interface::{Linker, create_and_enter_global_ctxt, passes, run_compiler};
use rustc_interface::interface::Result as CompilerResult;
use rustc_lint_defs::Level as LintLevel;
use rustc_session::config::{Input, OutputFilenames};

use crate::config::Config;
use crate::inject::inject_runtime_crate_and_deps;
use crate::passes::base_compiler_config;
use crate::passes::analysis::AnalysisPassResult;
use crate::passes::external_mutant::specialized_crate::SpecializedMutantCrateCompilationResult;

pub struct CompilationPassResult {
    pub duration: Duration,
    pub outputs: Arc<OutputFilenames>,
}

pub fn run(config: &Config, analysis_pass: &AnalysisPassResult, specialized_external_mutant_crate: Option<&(String, SpecializedMutantCrateCompilationResult)>) -> CompilerResult<CompilationPassResult> {
    let mut compiler_config = base_compiler_config(config);
    compiler_config.input = Input::Str {
        name: compiler_config.input.source_name(),
        input: analysis_pass.generated_crate_code.to_owned(),
    };

    // The generated crate code relies on the rustc test harness using a custom test runner.
    compiler_config.opts.test = config.opts.crate_kind.provides_tests();
    // The generated crate code uses many unstable and internal features, most of which are emitted by rustc itself.
    compiler_config.opts.unstable_features = UnstableFeatures::Allow;
    // Disable lints on generated crate code.
    compiler_config.opts.lint_cap = Some(LintLevel::Allow);

    // The generated crate code relies on the `mutest_runtime` crate (and its dependencies), which must be loaded.
    inject_runtime_crate_and_deps(config, &mut compiler_config, specialized_external_mutant_crate);

    let compilation_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<CompilationPassResult> {
        let t_start = Instant::now();

        let sess = &compiler.sess;
        let codegen_backend = &*compiler.codegen_backend;

        let krate = passes::parse(sess);

        let (linker, outputs) = create_and_enter_global_ctxt(compiler, krate, |tcx| {
            let _ = tcx.resolver_for_lowering();

            passes::write_dep_info(tcx);

            passes::write_interface(tcx);

            tcx.ensure_ok().analysis(());

            let outputs = tcx.output_filenames(()).clone();
            let linker = Linker::codegen_and_build_linker(tcx, &*compiler.codegen_backend);

            (linker, outputs)
        });

        linker.link(sess, codegen_backend);

        Ok(CompilationPassResult {
            duration: t_start.elapsed(),
            outputs,
        })
    })?;

    Ok(compilation_pass)
}
