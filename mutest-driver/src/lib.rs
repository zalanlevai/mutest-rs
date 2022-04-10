#![feature(rustc_private)]
extern crate rustc_ast_pretty;
extern crate rustc_errors;
extern crate rustc_feature;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

pub mod config;
pub mod passes;

use rustc_interface::interface::Result as CompilerResult;

use crate::config::Config;

pub fn main(config: Config) -> CompilerResult<()> {
    let sysroot = passes::get_sysroot();
    println!("rustc sysroot: {}", sysroot.display());

    let analysis_pass = passes::analysis::run(&config, sysroot.to_owned())?;
    println!("{}", config.package_directory_path.join("src/main.rs").display());
    println!("```\n{}```", analysis_pass.generated_crate_code);

    let compilation_pass = passes::compilation::run(&config, sysroot.to_owned(), &analysis_pass)?;
    println!("{:#?}", compilation_pass.outputs);

    Ok(())
}
