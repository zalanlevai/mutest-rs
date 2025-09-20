#![feature(decl_macro)]
#![feature(if_let_guard)]
#![feature(iter_array_chunks)]
#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]

#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_feature;
extern crate rustc_graphviz;
extern crate rustc_hash;
extern crate rustc_interface;
extern crate rustc_lint_defs;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

extern crate itertools;
extern crate smallvec;
extern crate thin_vec;

pub mod config;
pub mod inject;
pub mod passes;
pub mod print;
pub mod write;

use std::time::Instant;

use rustc_interface::interface::Result as CompilerResult;

use crate::config::Config;
use crate::passes::analysis::AnalysisPassResult;
use crate::passes::compilation::CompilationPassResult;
use crate::passes::external_mutant::specialized_crate::SpecializedMutantCrateCompilationResult;
use crate::write::write_timings;

pub struct RunResult {
    pub analysis_pass: Option<AnalysisPassResult>,
    pub specialized_external_mutant_pass: Option<SpecializedMutantCrateCompilationResult>,
    pub compilation_pass: Option<CompilationPassResult>,
}

pub fn run(mut config: Config) -> CompilerResult<RunResult> {
    let mut run_result = RunResult {
        analysis_pass: None,
        specialized_external_mutant_pass: None,
        compilation_pass: None,
    };

    let t_start = Instant::now();

    let original_print_opts = config.opts.print_opts.clone();

    #[cfg(feature = "embed-runtime")]
    inject::extract_runtime_crate_and_deps(&config.target_dir_root());

    let Some(mut analysis_pass) = passes::analysis::run(&mut config)? else { return Ok(run_result) };

    if let Some(_) = config.opts.print_opts.code.take() {
        if config.opts.print_opts.print_headers { println!("\n@@@ code @@@\n"); }
        println!("{}", analysis_pass.generated_crate_code);
        if config.opts.print_opts.print_headers { println!(); }
        if let config::Mode::Print = config.opts.mode && config.opts.print_opts.is_empty() {
            if let Some(write_opts) = &config.opts.write_opts {
                write_timings(write_opts, t_start.elapsed(), &analysis_pass, None, None);
            }
            if config.opts.report_timings {
                println!("finished in {total:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; batching {batching:.2?}; codegen {codegen:.2?}; write {write:.2?})",
                    total = analysis_pass.duration,
                    targets = analysis_pass.test_discovery_duration + analysis_pass.target_analysis_duration,
                    mutations = analysis_pass.mutation_generation_duration,
                    batching = analysis_pass.mutation_conflict_resolution_duration + analysis_pass.mutation_batching_duration,
                    codegen = analysis_pass.codegen_duration,
                    write = analysis_pass.write_duration,
                );
            }
            run_result.analysis_pass = Some(analysis_pass);
            return Ok(run_result);
        }
    }

    let mut specialized_external_mutant_crate = None;
    if let Some((crate_name, specialized_mutant_crate_request)) = analysis_pass.specialized_external_mutant_crate.take() {
        if config.opts.verbosity >= 1 {
            println!("generating specialized mutant crate for external tests");
        }

        let pass_result = passes::external_mutant::specialized_crate::compile_specialized_mutant_crate(&config, original_print_opts, specialized_mutant_crate_request)?;
        specialized_external_mutant_crate = Some((crate_name, pass_result));
    }

    let compilation_pass = passes::compilation::run(&config, &analysis_pass, specialized_external_mutant_crate.as_ref())?;

    if config.opts.report_timings {
        if let Some(write_opts) = &config.opts.write_opts {
            let specialized_external_mutant_pass = specialized_external_mutant_crate.as_ref().map(|(_, pass)| pass);
            write_timings(write_opts, t_start.elapsed(), &analysis_pass, specialized_external_mutant_pass, Some(&compilation_pass));
        }
        println!("finished in {total:.2?}",
            total = t_start.elapsed(),
        );

        match &specialized_external_mutant_crate {
            Some((_, specialized_external_mutant_pass)) => {
                let Some(specialized_external_mutant_analysis_pass) = &specialized_external_mutant_pass.nested_run_result.analysis_pass else { unreachable!() };
                println!("analysis took {analysis:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; batching {batching:.2?}; hygiene {hygiene:.2?}; codegen {codegen:.2?}; write {write:.2?})",
                    analysis = analysis_pass.duration + specialized_external_mutant_analysis_pass.duration,
                    targets = analysis_pass.test_discovery_duration + analysis_pass.target_analysis_duration,
                    mutations = specialized_external_mutant_analysis_pass.mutation_generation_duration,
                    batching = specialized_external_mutant_analysis_pass.mutation_conflict_resolution_duration + specialized_external_mutant_analysis_pass.mutation_batching_duration,
                    hygiene = analysis_pass.sanitize_macro_expns_duration + specialized_external_mutant_analysis_pass.sanitize_macro_expns_duration,
                    codegen = analysis_pass.codegen_duration + specialized_external_mutant_analysis_pass.codegen_duration,
                    write = analysis_pass.write_duration + specialized_external_mutant_analysis_pass.write_duration,
                );
            }
            None => {
                println!("analysis took {analysis:.2?} (targets {targets:.2?}; mutations {mutations:.2?}; batching {batching:.2?}; hygiene {hygiene:.2?}; codegen {codegen:.2?}; write {write:.2?})",
                    analysis = analysis_pass.duration,
                    targets = analysis_pass.test_discovery_duration + analysis_pass.target_analysis_duration,
                    mutations = analysis_pass.mutation_generation_duration,
                    batching = analysis_pass.mutation_conflict_resolution_duration + analysis_pass.mutation_batching_duration,
                    hygiene = analysis_pass.sanitize_macro_expns_duration,
                    codegen = analysis_pass.codegen_duration,
                    write = analysis_pass.write_duration,
                );
            }
        };

        match &specialized_external_mutant_crate {
            Some((_, specialized_external_mutant_pass)) => {
                let Some(specialized_external_mutant_compilation_pass) = &specialized_external_mutant_pass.nested_run_result.compilation_pass else { unreachable!() };
                println!("compilations took {total:.2?} (mutant {mutant:.2?}, tests {tests:.2?})",
                    total = specialized_external_mutant_compilation_pass.duration + compilation_pass.duration,
                    mutant = specialized_external_mutant_compilation_pass.duration,
                    tests = compilation_pass.duration,
                );
            }
            None => {
                println!("compilation took {compilation:.2?}",
                    compilation = compilation_pass.duration,
                );
            }
        }
    }

    run_result.analysis_pass = Some(analysis_pass);
    run_result.specialized_external_mutant_pass = specialized_external_mutant_crate.map(|(_, pass)| pass);
    run_result.compilation_pass = Some(compilation_pass);
    Ok(run_result)
}
