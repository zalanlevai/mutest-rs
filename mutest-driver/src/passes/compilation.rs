use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

use rustc_feature::UnstableFeatures;
use rustc_interface::{Linker, create_and_enter_global_ctxt, passes, run_compiler};
use rustc_interface::interface::Result as CompilerResult;
use rustc_lint_defs::Level as LintLevel;
use rustc_session::EarlyDiagCtxt;
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
    let early_dcx = EarlyDiagCtxt::new(compiler_config.opts.error_format);
    let sysroot = &compiler_config.opts.sysroot;
    let triple = &compiler_config.opts.target_triple;
    compiler_config.opts.search_paths.push(SearchPath::from_cli_opt(sysroot, &triple, &early_dcx, &format!("crate={}", config.mutest_search_path.display()), true));
    compiler_config.opts.search_paths.push(SearchPath::from_cli_opt(sysroot, &triple, &early_dcx, &format!("dependency={}", config.mutest_search_path.join("deps").display()), true));
    // The externs (paths to dependencies) of the `mutest_runtime` crate are baked into it at compile time.
    // These must be propagated to any crate which depends on it.
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

        // NOTE: Only inject extern dependencies of mutest-runtime with the
        //       `__mutest_runtime_public_dep_` prefix, which is used both
        //       to denote an exposed dependency that needs to be injected into the generated code,
        //       and to avoid potential name collisions with the crate's own externs.
        if !name.starts_with("__mutest_runtime_public_dep_") { continue; }

        let existing_extern = externs.insert(name.to_owned(), ExternEntry {
            location: match path {
                Some(path) => ExternLocation::ExactPaths(BTreeSet::from([
                    CanonicalizedPath::new(config.mutest_search_path.join(path)),
                ])),
                None => ExternLocation::FoundInLibrarySearchDirectories,
            },
            is_private_dep,
            add_prelude,
            nounused_dep,
            force,
        });

        if let Some(_existing_extern) = existing_extern {
            let mut diag = early_dcx.early_struct_fatal(format!("mutest-injected crate conflicts with existing extern `{name}`"));
            diag.note("mutest-injected crates use the reserved `__mutest_runtime_public_dep_` prefix: if you see this error for any other crate, please file a bug report");
            diag.emit();
        }
    }
    compiler_config.opts.externs = Externs::new(externs);
    drop(early_dcx);

    let compilation_pass = run_compiler(compiler_config, |compiler| -> CompilerResult<CompilationPassResult> {
        let t_start = Instant::now();

        let sess = &compiler.sess;
        let codegen_backend = &*compiler.codegen_backend;

        let krate = passes::parse(sess);

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

        Ok(CompilationPassResult {
            duration: t_start.elapsed(),
            outputs,
        })
    })?;

    Ok(compilation_pass)
}
