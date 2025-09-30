use std::collections::BTreeMap;
use std::env;
use std::sync::Arc;
use std::time::{Duration, Instant};

use rustc_interface::interface::Result as CompilerResult;
use rustc_session::config::OutputFilenames;

use crate::config::{self, Config};
use crate::passes::parse_compiler_args;
use crate::passes::analysis::AnalysisPassResult;
use crate::passes::compilation::CompilationPassResult;
use crate::passes::external_mutant::{RustcInvocation, ExternalTargets};

pub struct SpecializedMutantCrateCompilationRequest {
    pub rustc_invocation: RustcInvocation,
    pub specialized_extra_filename: String,
    pub external_targets: ExternalTargets,
}

pub struct NestedRunResult {
    pub analysis_pass: Option<AnalysisPassResult>,
    pub compilation_pass: Option<CompilationPassResult>,
}

pub struct SpecializedMutantCrateCompilationResult {
    pub nested_run_result: NestedRunResult,
    pub duration: Duration,
    pub outputs: Arc<OutputFilenames>,
}

pub fn compile_specialized_mutant_crate(
    config: &Config,
    print_opts: config::PrintOptions,
    request: SpecializedMutantCrateCompilationRequest,
) -> CompilerResult<SpecializedMutantCrateCompilationResult> {
    let t_start = Instant::now();

    // Set environment variables up as they were in the original compilation,
    // and keep track of the changes so we can restore the environment afterwards.
    let mut added_env_vars = vec![];
    let mut modified_env_vars = vec![];
    for (var, target_val) in &request.rustc_invocation.env_vars {
        match env::var(var).ok() {
            Some(val) => modified_env_vars.push((var.clone(), val)),
            None => added_env_vars.push(var.clone()),
        }
        // SAFETY: No other thread is running, as we are not in a compilation session.
        unsafe { env::set_var(var, target_val) };
    }
    let mut removed_env_vars = std::env::vars().collect::<BTreeMap<_, _>>();
    for (var, _) in &request.rustc_invocation.env_vars {
        removed_env_vars.remove(var);
    }
    for var in removed_env_vars.keys() {
        // SAFETY: No other thread is running, as we are not in a compilation session.
        unsafe { env::remove_var(var) };
    }

    let mut compiler_config = parse_compiler_args(&request.rustc_invocation.args).expect("no compiler configuration was generated");

    compiler_config.opts.cg.extra_filename = format!("{}{}", compiler_config.opts.cg.extra_filename, request.specialized_extra_filename);

    let mutant_config = Config {
        compiler_config,
        invocation_fingerprint: None,
        mutest_target_dir_root: config.mutest_target_dir_root.clone(),
        mutest_search_path: config.mutest_search_path.clone(),
        opts: config::Options {
            crate_kind: config::CrateKind::MutantForExternalTests(request.external_targets),
            // NOTE: While we are compiling a different Cargo target, we treat it like the original.
            //       This is mainly used to ensure that JSON metadata files appear together,
            //       as the two crates represent a single pass.
            cargo_target_kind: config.opts.cargo_target_kind,

            mode: config::Mode::Build,
            verbosity: config.opts.verbosity,
            // NOTE: We report timings for this compilation pass separately.
            report_timings: false,
            print_opts,
            unsafe_targeting: config.opts.unsafe_targeting,
            operators: config.opts.operators,
            call_graph_depth_limit: config.opts.call_graph_depth_limit,
            call_graph_trace_length_limit: config.opts.call_graph_trace_length_limit,
            mutation_depth: config.opts.mutation_depth,
            mutation_parallelism: config.opts.mutation_parallelism.clone(),

            write_opts: config.opts.write_opts.clone(),
            verify_opts: config.opts.verify_opts.clone(),
            sanitize_macro_expns: config.opts.sanitize_macro_expns,
        },
    };

    let run_result = crate::run(mutant_config);

    // Restore the environment by undoing the changes to the environment variables.
    for (var, val) in removed_env_vars {
        // SAFETY: No other thread is running, as we are not in a compilation session.
        unsafe { env::set_var(var, val) };
    }
    for (var, val) in modified_env_vars {
        // SAFETY: No other thread is running, as we are not in a compilation session.
        unsafe { env::set_var(var, val) };
    }
    for var in added_env_vars {
        // SAFETY: No other thread is running, as we are not in a compilation session.
        unsafe { env::remove_var(var) };
    }

    let run_result = run_result?;
    let Some(compilation_pass) = &run_result.compilation_pass else { unreachable!("no compiled output from specialized mutant crate") };
    let outputs = compilation_pass.outputs.clone();

    Ok(SpecializedMutantCrateCompilationResult {
        nested_run_result: NestedRunResult {
            analysis_pass: run_result.analysis_pass,
            compilation_pass: run_result.compilation_pass,
        },
        duration: t_start.elapsed(),
        outputs,
    })
}
