use std::convert::Infallible;
use std::ops::{ControlFlow, FromResidual, Residual, Try};
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

pub enum Flow<T, E> {
    Continue(T),
    Break,
    Err(E),
}

impl<T, E> Try for Flow<T, E> {
    type Output = T;
    type Residual = Flow<Infallible, E>;

    fn from_output(output: Self::Output) -> Self {
        Self::Continue(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Continue(value) => ControlFlow::Continue(value),
            Self::Break => ControlFlow::Break(Flow::Break),
            Self::Err(error) => ControlFlow::Break(Flow::Err(error)),
        }
    }
}

impl<T, E> Residual<T> for Flow<Infallible, E> {
    type TryType = Flow<T, E>;
}

impl<T, E, F: From<E>> FromResidual<Flow<Infallible, E>> for Flow<T, F> {
    fn from_residual(residual: Flow<Infallible, E>) -> Self {
        match residual {
            Flow::Continue(_) => unreachable!(),
            Flow::Break => Flow::Break,
            Flow::Err(error) => Flow::Err(From::from(error)),
        }
    }
}

impl<T, E, F: From<E>> FromResidual<Result<Infallible, E>> for Flow<T, F> {
    fn from_residual(residual: Result<Infallible, E>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(error) => Flow::Err(From::from(error)),
        }
    }
}

impl<T, E, F: From<E>> From<Flow<T, E>> for Result<Option<T>, F> {
    fn from(flow: Flow<T, E>) -> Self {
        match flow {
            Flow::Continue(value) => Ok(Some(value)),
            Flow::Break => Ok(None),
            Flow::Err(error) => Err(From::from(error)),
        }
    }
}

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
