use std::path::PathBuf;
use std::process;
use std::str;

use rustc_errors::registry::Registry;
use rustc_interface::Config as CompilerConfig;
use rustc_session::DiagnosticOutput;
use rustc_session::config::{Input, Options};
use rustc_span::edition::Edition;
use rustc_span::source_map::RealFileLoader;

use crate::config::Config;

pub fn get_sysroot() -> PathBuf {
    let sysroot_out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output().unwrap();

    PathBuf::from(str::from_utf8(&sysroot_out.stdout).unwrap().trim())
}

pub fn common_compiler_config(_config: &Config, sysroot: PathBuf, input: Input) -> CompilerConfig {
    CompilerConfig {
        opts: Options {
            edition: Edition::Edition2021,

            // search_paths: vec![
            //     SearchPath::from_sysroot_and_triple(Path::new("/Users/zalanlevai/.rustup/toolchains/nightly-2022-03-05-aarch64-apple-darwin"), "aarch64-apple-darwin"),
            // ],
            maybe_sysroot: Some(sysroot),

            ..Default::default()
        },

        crate_cfg: Default::default(),
        crate_check_cfg: Default::default(),

        input,
        input_path: None,
        output_dir: None,
        output_file: None,
        file_loader: Some(Box::new(RealFileLoader)),
        diagnostic_output: DiagnosticOutput::Default,

        lint_caps: Default::default(),

        parse_sess_created: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,

        registry: Registry::new(Default::default()),
    }
}

pub mod analysis;
pub mod compilation;
