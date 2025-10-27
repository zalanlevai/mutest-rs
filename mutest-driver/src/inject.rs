use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

use rustc_interface::Config as CompilerConfig;
use rustc_session::EarlyDiagCtxt;
use rustc_session::config::{ExternEntry, ExternLocation, Externs, OutputType, build_target_config, host_tuple};
use rustc_session::search_paths::{PathKind, SearchPath};
use rustc_session::utils::CanonicalizedPath;

use crate::config::Config;
use crate::passes::external_mutant::specialized_crate::SpecializedMutantCrateCompilationResult;

mod rlib_catalog { include!(env!("RLIB_CATALOG")); }

#[cfg(feature = "embed-runtime")]
fn extract_file(path: &Path, content: &[u8]) {
    let existing_file_up_to_date = match fs::read(path) {
        Ok(file_content) => file_content == content,
        Err(_) => false,
    };

    if existing_file_up_to_date { return; }

    fs::write(path, content).expect(&format!("cannot write file `{}`", path.display()));
}

const MUTEST_EXTRACTED_DEPS_DIR_NAME: &str = "mutest_deps";

#[cfg(feature = "embed-runtime")]
pub fn extract_runtime_crate_and_deps(target_dir_root_path: &Path) {
    let mutest_deps_dir_path = target_dir_root_path.join(MUTEST_EXTRACTED_DEPS_DIR_NAME);
    fs::create_dir_all(&mutest_deps_dir_path).expect(&format!("cannot create directory `{}`", mutest_deps_dir_path.display()));

    extract_file(&mutest_deps_dir_path.join(rlib_catalog::MUTEST_RUNTIME_RLIB_FILENAME), rlib_catalog::MUTEST_RUNTIME_RLIB_DATA);
    for (dep_file_name, dep_data) in rlib_catalog::MUTEST_RUNTIME_EXTERN_DEPS_DATA {
        extract_file(&mutest_deps_dir_path.join(dep_file_name), dep_data);
    }
}

const COMPILETIME_ARTIFACTS_DIR: &str = env!("COMPILETIME_ARTIFACTS_DIR");
const COMPILETIME_DEPS_DIR: &str = env!("COMPILETIME_DEPS_DIR");

pub fn inject_runtime_crate_and_deps(config: &Config, compiler_config: &mut CompilerConfig, specialized_external_mutant_crate: Option<&(String, SpecializedMutantCrateCompilationResult)>) {
    let early_dcx = EarlyDiagCtxt::new(compiler_config.opts.error_format);

    let host_triple = host_tuple();
    let target_triple = compiler_config.opts.target_triple.tuple();

    let mutest_host_artifacts_dir_path = if cfg!(feature = "embed-runtime") {
        &config.target_dir_root().join(MUTEST_EXTRACTED_DEPS_DIR_NAME)
    } else {
        config.mutest_search_path.as_deref().unwrap_or(Path::new(COMPILETIME_ARTIFACTS_DIR))
    };

    let mutest_host_deps_dir_path = if cfg!(feature = "embed-runtime") {
        mutest_host_artifacts_dir_path
    } else {
        match &config.mutest_search_path {
            Some(path) => &path.join("deps"),
            None => Path::new(COMPILETIME_DEPS_DIR),
        }
    };

    let mutest_target_artifacts_dir_path = match target_triple == host_triple {
        true => mutest_host_artifacts_dir_path,
        false => {
            if cfg!(feature = "embed-runtime") {
                let mut diag = early_dcx.early_struct_fatal("mutest-rs with runtime embedding does not support cross-compilation");
                diag.note(format!("target: `{target_triple}`"));
                diag.emit();
            }

            let profile = mutest_host_artifacts_dir_path.file_name().expect("invalid mutest search path");
            let root_dir_path = mutest_host_artifacts_dir_path.parent().expect("invalid mutest search path");

            &root_dir_path.join(target_triple).join(profile)
        }
    };

    let mutest_target_deps_dir_path = match target_triple == host_triple {
        true => mutest_host_deps_dir_path,
        false => &mutest_target_artifacts_dir_path.join("deps"),
    };

    if target_triple != host_triple {
        compiler_config.opts.search_paths.push(SearchPath::new(PathKind::Dependency, mutest_target_deps_dir_path.to_owned()));
    }
    // NOTE: We need the host dependencies for procedural macro crate dependencies, as these run on the host, during compilation.
    compiler_config.opts.search_paths.push(SearchPath::new(PathKind::Dependency, mutest_host_deps_dir_path.to_owned()));

    // The externs (paths to dependencies) of the `mutest_runtime` crate are baked into it at compile time.
    // These must be propagated to any crate which depends on it.
    let mut externs = BTreeMap::<String, ExternEntry>::new();
    for (key, entry) in compiler_config.opts.externs.iter() {
        externs.insert(key.clone(), entry.clone());
    }

    if !config.opts.embedded {
        externs.insert("mutest_runtime".to_owned(), ExternEntry {
            location: ExternLocation::ExactPaths(BTreeSet::from([
                CanonicalizedPath::new(mutest_target_artifacts_dir_path.join(rlib_catalog::MUTEST_RUNTIME_RLIB_FILENAME)),
            ])),
            is_private_dep: false,
            add_prelude: false,
            nounused_dep: false,
            force: false,
        });
    } else {
        let rlib_path = mutest_target_artifacts_dir_path.join("libmutest_runtime_embedded_target_stub.rlib");
        match fs::exists(&rlib_path) {
            Ok(true) => {}
            // NOTE: We let the compiler emit its usual error message upon being unable to read an rlib file path.
            Err(_) => {}

            Ok(false) => {
                let profile = mutest_host_artifacts_dir_path.file_name().expect("invalid mutest search path");

                let mut diag = match target_triple == host_triple {
                    true => early_dcx.early_struct_fatal(format!("cannot find mutest-rs embedded runtime for host")),
                    false => early_dcx.early_struct_fatal(format!("cannot find cross-compiled mutest-rs embedded runtime for target `{target_triple}`")),
                };
                diag.note(format!("searching in `{}`", mutest_target_artifacts_dir_path.display()));
                match target_triple == host_triple {
                    true => {
                        // NOTE: We remove the explicit `--target` argument because if specified, Cargo will place the host artifact in a target triple subdirectory.
                        diag.note(format!("consider running `cargo build --profile={} -p mutest-runtime-embedded-target-stub` in the mutest-rs source tree", profile.display()));
                    }
                    false => {
                        diag.note(format!("consider running `cargo build --target={} --profile={} -p mutest-runtime-embedded-target-stub` in the mutest-rs source tree", target_triple, profile.display()));
                    }
                }
                diag.emit();
            }
        }
        externs.insert("mutest_runtime".to_owned(), ExternEntry {
            location: ExternLocation::ExactPaths(BTreeSet::from([
                CanonicalizedPath::new(rlib_path),
            ])),
            is_private_dep: false,
            add_prelude: false,
            nounused_dep: false,
            force: false,
        });
    }

    for &(visible_crate_name, dep_file_name) in rlib_catalog::MUTEST_RUNTIME_PUBLIC_DEPS {
        let mut dep_file_paths = BTreeSet::new();
        // FIXME: Use the actual public dependency list of the injected embedded runtime crate,
        //        rather than piggy-backing off the main mutest-runtime crate.
        if !config.opts.embedded {
            dep_file_paths.insert(CanonicalizedPath::new(mutest_target_deps_dir_path.join(dep_file_name)));
        } else {
            let dep_file_name_root = match dep_file_name.split_once("-") {
                // lib<NAME>-<HASH>.<EXTENSION>
                Some((dep_file_name_root, _)) => dep_file_name_root,
                None => match dep_file_name.split_once(".") {
                    // lib<NAME>.<EXTENSION>
                    Some((dep_file_name_root, _)) => dep_file_name_root,
                    None => dep_file_name,
                },
            };
            fs::read_dir(mutest_target_deps_dir_path).expect(&format!("cannot read directory: `{}`", mutest_target_deps_dir_path.display()))
                .filter_map(|dir_entry| {
                    let dir_entry = dir_entry.ok()?;
                    let file_name = dir_entry.file_name().into_string().ok()?;
                    let file_name_root = match file_name.split_once("-") {
                        // lib<NAME>-<HASH>.<EXTENSION>
                        Some((file_name_root, _)) => file_name_root,
                        None => match dep_file_name.split_once(".") {
                            // lib<NAME>.<EXTENSION>
                            Some((file_name_root, _)) => file_name_root,
                            None => &file_name,
                        },
                    };
                    if file_name_root != dep_file_name_root { return None; }
                    Some(CanonicalizedPath::new(dir_entry.path()))
                })
                .collect_into(&mut dep_file_paths);
        }
        let existing_extern = externs.insert(visible_crate_name.to_owned(), ExternEntry {
            location: ExternLocation::ExactPaths(dep_file_paths),
            is_private_dep: false,
            add_prelude: false,
            nounused_dep: false,
            force: false,
        });
        if let Some(_existing_extern) = existing_extern {
            let mut diag = early_dcx.early_struct_fatal(format!("mutest-injected crate conflicts with existing extern `{visible_crate_name}`"));
            diag.note("mutest-injected crates use the reserved `__mutest_runtime_public_dep_` prefix: if you see this error for any other crate, please file a bug report");
            diag.emit();
        }
    }

    if let Some((visible_crate_name, specialized_external_mutant_crate_compilation)) = specialized_external_mutant_crate {
        let mut file_path = specialized_external_mutant_crate_compilation.outputs.path(OutputType::Metadata).as_path().to_owned();
        file_path.set_extension("rlib");

        let Some(extern_entry) = externs.get_mut(visible_crate_name) else {
            early_dcx.early_fatal(format!("cannot find extern `{visible_crate_name}` to replace with specialized external mutant crate"));
        };

        extern_entry.location = ExternLocation::ExactPaths(BTreeSet::from([
            CanonicalizedPath::new(file_path),
        ]));
    }

    compiler_config.opts.externs = Externs::new(externs);
}

pub fn inject_test_crate_shim_if_no_target_std(config: &Config, compiler_config: &mut CompilerConfig) {
    let early_dcx = EarlyDiagCtxt::new(compiler_config.opts.error_format);

    let target = build_target_config(&early_dcx, &compiler_config.opts.target_triple, &compiler_config.opts.sysroot);
    if target.metadata.std == Some(true) { return; }

    let target_triple = compiler_config.opts.target_triple.tuple();

    if !config.opts.embedded {
        let mut diag = early_dcx.early_struct_fatal(format!("target `{}` does not support std, but the embedded mutation runtime was not specified", target_triple));
        diag.note("the default mutation runtime does not support targets without std support");
        diag.note("consider running with the `--Zembedded` flag to use the embedded mutation runtime");
        diag.emit();
    }

    if cfg!(feature = "embed-runtime") {
        let mut diag = early_dcx.early_struct_fatal("mutest-rs with runtime embedding does not support cross-compilation");
        diag.note(format!("target: `{target_triple}`"));
        diag.emit();
    }

    let mutest_host_artifacts_dir_path = config.mutest_search_path.as_deref().unwrap_or(Path::new(COMPILETIME_ARTIFACTS_DIR));

    let mutest_target_artifacts_dir_path = {
        let profile = mutest_host_artifacts_dir_path.file_name().expect("invalid mutest search path");
        let root_dir_path = mutest_host_artifacts_dir_path.parent().expect("invalid mutest search path");

        &root_dir_path.join(target_triple).join(profile)
    };

    let mut externs = BTreeMap::<String, ExternEntry>::new();
    for (key, entry) in compiler_config.opts.externs.iter() {
        externs.insert(key.clone(), entry.clone());
    }

    let rlib_path = mutest_target_artifacts_dir_path.join("libtest_metadata_shim.rlib");
    match fs::exists(&rlib_path) {
        Ok(true) => {}
        // NOTE: We let the compiler emit its usual error message upon being unable to read an rlib file path.
        Err(_) => {}

        Ok(false) => {
            let profile = mutest_host_artifacts_dir_path.file_name().expect("invalid mutest search path");

            let mut diag = early_dcx.early_struct_fatal(format!("cannot find cross-compiled libtest metadata shim for target `{target_triple}`"));
            diag.note(format!("searching in `{}`", mutest_target_artifacts_dir_path.display()));
            diag.note(format!("consider running `cargo build --target={} --profile={} -p test-metadata-shim` in the mutest-rs source tree", target_triple, profile.display()));
            diag.emit();
        }
    }
    externs.insert("test".to_owned(), ExternEntry {
        location: ExternLocation::ExactPaths(BTreeSet::from([
            CanonicalizedPath::new(rlib_path),
        ])),
        is_private_dep: false,
        add_prelude: false,
        nounused_dep: false,
        force: false,
    });

    compiler_config.opts.externs = Externs::new(externs);
}
