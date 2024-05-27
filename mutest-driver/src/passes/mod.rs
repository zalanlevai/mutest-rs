use std::convert::Infallible;
use std::ops::{ControlFlow, FromResidual, Residual, Try};
use std::sync::Arc;
use std::sync::atomic::{self, AtomicBool};

use rustc_interface::Config as CompilerConfig;
use rustc_interface::interface::Result as CompilerResult;
use rustc_session::config::Input;
use rustc_session::parse::ParseSess;
use rustc_span::Symbol;
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

pub fn copy_compiler_settings(config: &CompilerConfig) -> CompilerConfig {
    let input = match &config.input {
        Input::File(f) => Input::File(f.clone()),
        Input::Str { name, input } => Input::Str { name: name.clone(), input: input.clone() },
    };

    CompilerConfig {
        opts: config.opts.clone(),
        crate_cfg: config.crate_cfg.clone(),
        crate_check_cfg: config.crate_check_cfg.clone(),
        input,
        output_file: config.output_file.clone(),
        output_dir: config.output_dir.clone(),
        ice_file: config.ice_file.clone(),
        file_loader: Some(Box::new(RealFileLoader)),
        locale_resources: config.locale_resources,
        lint_caps: config.lint_caps.clone(),
        psess_created: None,
        hash_untracked_state: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,
        registry: rustc_driver::diagnostics_registry(),
        using_internal_features: Arc::new(AtomicBool::new(config.using_internal_features.load(atomic::Ordering::Relaxed))),
        expanded_args: config.expanded_args.clone(),
    }
}

struct RustcConfigCallbacks {
    config: Option<CompilerConfig>,
}

impl rustc_driver::Callbacks for RustcConfigCallbacks {
    fn config(&mut self, config: &mut CompilerConfig) {
        self.config = Some(copy_compiler_settings(config));
    }

    fn after_crate_root_parsing<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        _queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        rustc_driver::Compilation::Stop
    }
}

pub fn parse_compiler_args(args: &[String]) -> CompilerResult<Option<CompilerConfig>> {
    let mut callbacks = RustcConfigCallbacks { config: None };
    rustc_driver::RunCompiler::new(args, &mut callbacks).run()?;
    Ok(callbacks.config)
}

pub fn track_invocation_fingerprint(parse_sess: &mut ParseSess, invocation_fingerprint: &Option<String>) {
    parse_sess.env_depinfo.get_mut().insert((
        Symbol::intern("MUTEST_FINGERPRINT"),
        invocation_fingerprint.as_deref().map(Symbol::intern),
    ));
}

pub fn base_compiler_config(config: &Config) -> CompilerConfig {
    let mut compiler_config = copy_compiler_settings(&config.compiler_config);

    let invocation_fingerprint = config.invocation_fingerprint.clone();
    compiler_config.psess_created = Some(Box::new(move |parse_sess| {
        track_invocation_fingerprint(parse_sess, &invocation_fingerprint);
    }));

    // Register #[cfg(mutest)] as a valid cfg.
    compiler_config.crate_check_cfg.push("cfg(mutest, values(none()))".to_owned());
    // Enable #[cfg(mutest)].
    compiler_config.crate_cfg.push("mutest".to_owned());

    compiler_config
}

pub mod analysis;
pub mod compilation;
