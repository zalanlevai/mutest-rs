#![feature(let_else)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]

#![feature(rustc_private)]
extern crate rustc_ast_pretty;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_feature;
extern crate rustc_interface;
extern crate rustc_lint_defs;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

pub mod cli;
pub mod config;
pub mod passes;

use rustc_interface::interface::Result as CompilerResult;

use crate::config::Config;

pub fn run(config: Config) -> CompilerResult<()> {
    let Some(analysis_pass) = passes::analysis::run(&config)? else { return Ok(()) };

    if let config::Mode::PrintCode = config.opts.mode {
        println!("{}", analysis_pass.generated_crate_code);
        return Ok(());
    }

    let _compilation_pass = passes::compilation::run(&config, &analysis_pass)?;

    Ok(())
}
