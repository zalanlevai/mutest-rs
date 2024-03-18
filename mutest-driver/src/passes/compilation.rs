use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

use rustc_feature::UnstableFeatures;
use rustc_interface::interface::Result as CompilerResult;
use rustc_interface::run_compiler;
use rustc_lint_defs::Level as LintLevel;
use rustc_session::EarlyErrorHandler;
use rustc_session::config::{ExternEntry, ExternLocation, Externs, Input, OutputFilenames};
use rustc_session::search_paths::SearchPath;
use rustc_session::utils::CanonicalizedPath;

use crate::config::Config;
use crate::passes::base_compiler_config;
use crate::passes::analysis::AnalysisPassResult;

pub struct CompilationPassResult {
    pub duration: Duration,
    pub outputs: Arc<OutputFilenames>,
}

pub fn run(config: &Config, analysis_pass: &AnalysisPassResult) -> CompilerResult<CompilationPassResult> {
    let mut compiler_config = base_compiler_config(config);
    compiler_config.input = Input::Str {
        name: compiler_config.input.source_name(),
        input: analysis_pass.generated_crate_code.to_owned(),
    };

    // The generated crate code relies on the rustc test harness using a custom test runner.
    compiler_config.opts.test = true;
    // The generated crate code uses many unstable and internal features, most of which are emitted by rustc itself.
    compiler_config.opts.unstable_features = UnstableFeatures::Allow;
    // Disable lints on generated crate code.
    compiler_config.opts.lint_cap = Some(LintLevel::Allow);

    // The generated crate code relies on the `mutest_runtime` crate (and its dependencies), which must be loaded.
    let early_error_handler = EarlyErrorHandler::new(compiler_config.opts.error_format);
    compiler_config.opts.search_paths.push(SearchPath::from_cli_opt(&early_error_handler, &format!("crate={}", config.mutest_search_path.display())));
    compiler_config.opts.search_paths.push(SearchPath::from_cli_opt(&early_error_handler, &format!("dependency={}", config.mutest_search_path.join("deps").display())));
    early_error_handler.abort_if_errors();
    // The externs (paths to dependencies) of the `mutest_runtime` crate are baked into it at compile time. These must
    // be propagated to any crate which depends on it.
    let mut externs = BTreeMap::<String, ExternEntry>::new();
    for (key, entry) in compiler_config.opts.externs.iter() {
        externs.insert(key.clone(), entry.clone());
    }
    for (prefix, name, path) in mutest_runtime::build::externs() {
        let mut is_private_dep = false;
        let mut add_prelude = true;
        let mut nounused_dep = false;
        let mut force = false;
        for option in prefix.iter().flat_map(|option| option.split(",").map(str::trim)) {
            match option {
                "priv" => is_private_dep = true,
                "noprelude" => add_prelude = false,
                "nounused" => nounused_dep = true,
                "force" => force = true,
                _ => unreachable!("unknown extern option `{option}`"),
            }
        }

        externs.insert(name.to_owned(), ExternEntry {
            location: match path {
                Some(path) => ExternLocation::ExactPaths(BTreeSet::from([
                    CanonicalizedPath::new(&config.mutest_search_path.join(path)),
                ])),
                None => ExternLocation::FoundInLibrarySearchDirectories,
            },
            is_private_dep,
            add_prelude,
            nounused_dep,
            force,
        });
    }
    // Externs for some std macros may have to be loaded.
    externs.insert("std_detect".to_owned(), ExternEntry {
        location: ExternLocation::FoundInLibrarySearchDirectories,
        is_private_dep: false,
        add_prelude: true,
        nounused_dep: false,
        force: false,
    });
    compiler_config.opts.externs = Externs::new(externs);

    let compilation_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<CompilationPassResult> {
        let t_start = Instant::now();

        let (linker, outputs) = compiler.enter(|queries| {
            queries.parse()?;

            let outputs = queries.global_ctxt()?.enter(|tcx| {
                let _ = tcx.resolver_for_lowering(());

                let outputs = tcx.output_filenames(());

                tcx.analysis(())?;

                Ok(outputs.clone())
            })?;

            let ongoing_codegen = queries.ongoing_codegen()?;

            let linker = queries.linker(ongoing_codegen)?;
            Ok((linker, outputs))
        })?;

        linker.link()?;

        Ok(CompilationPassResult {
            duration: t_start.elapsed(),
            outputs,
        })
    })?;

    Ok(compilation_pass)
}
