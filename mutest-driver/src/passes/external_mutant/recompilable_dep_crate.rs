use std::sync::Arc;
use std::time::{Duration, Instant};

use rustc_interface::{Linker, create_and_enter_global_ctxt, passes, run_compiler};
use rustc_interface::Config as CompilerConfig;
use rustc_interface::interface::Result as CompilerResult;
use rustc_session::config::{OptLevel, OutputFilenames};

use crate::passes::external_mutant::RustcInvocation;
use crate::passes::external_mutant::crate_const_storage;
use crate::passes::base_compiler_config_from_parts;

pub struct RecompilableDepCrateCompilationResult {
    pub duration: Duration,
    pub outputs: Arc<OutputFilenames>,
}

pub fn compile_recompilable_dep_crate(compiler_config: &CompilerConfig, rustc_invocation: &RustcInvocation) -> CompilerResult<RecompilableDepCrateCompilationResult> {
    let mut compiler_config = base_compiler_config_from_parts(compiler_config, None);

    // NOTE: Disable all MIR optimizations in all cases to ensure identical MIRs
    //       during analysis regardless of final optimization level.
    compiler_config.opts.optimize = OptLevel::No;
    // NOTE: We must disable MIR optimizations to disable inlining of function calls,
    //       which is necessary to building a complete call graph in all circumstances.
    //       The MIR generated in this pass is not used for the final compilation anyway.
    compiler_config.opts.unstable_opts.mir_opt_level = Some(0);
    // NOTE: Ensure that the MIR of all items is encoded, regardless of whether they are
    //       needed for linking or binary codegen.
    //       This is needed to create the full call graph from an external crate.
    compiler_config.opts.unstable_opts.always_encode_mir = true;

    let compilation_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<RecompilableDepCrateCompilationResult> {
        let t_start = Instant::now();

        let sess = &compiler.sess;
        let codegen_backend = &*compiler.codegen_backend;

        let mut krate = passes::parse(sess);

        // NOTE: We must register our custom tool attribute namespace before the
        //       relevant attribute validation is performed during macro expansion.
        mutest_emit::codegen::tool_attr::register(sess, &mut krate);

        crate_const_storage::embed_rustc_invocation(&mut krate, rustc_invocation);

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

        Ok(RecompilableDepCrateCompilationResult {
            duration: t_start.elapsed(),
            outputs,
        })
    })?;

    Ok(compilation_pass)
}
