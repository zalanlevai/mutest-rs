#![feature(let_else)]
#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]

#![feature(rustc_private)]
extern crate rustc_ast_pretty;
extern crate rustc_errors;
extern crate rustc_feature;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

pub mod config;
pub mod passes;

use std::process::Command;

use rustc_interface::interface::Result as CompilerResult;
use rustc_session::config::OutputType;

use crate::config::Config;

pub fn main(config: Config) -> CompilerResult<i32> {
    let sysroot = passes::get_sysroot();

    let Some(analysis_pass) = passes::analysis::run(&config, sysroot.to_owned())? else { return Ok(0) };

    if let config::Mode::PrintCode = config.opts.mode {
        println!("{}", analysis_pass.generated_crate_code);
        return Ok(0);
    }

    let compilation_pass = passes::compilation::run(&config, sysroot.to_owned(), &analysis_pass)?;

    if let config::Mode::Build = config.opts.mode {
        println!("{:#?}", compilation_pass.outputs);
        return Ok(0);
    }

    let config::Mode::BuildAndRun(passed_args) = config.opts.mode else { return Ok(0); };

    let program = compilation_pass.outputs.outputs
        .get(&OutputType::Exe).expect("compilation pass did not generate binary")
        .to_owned().expect("compilation pass did not generate binary");

    let command = Command::new(program)
        .args(passed_args)
        .spawn().expect("failed to run generated binary")
        .wait().expect("failed to run generated binary");

    Ok(command.code().unwrap_or(0))
}
