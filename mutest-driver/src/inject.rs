use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use rustc_interface::Config as CompilerConfig;
use rustc_session::EarlyDiagCtxt;
use rustc_session::config::{ExternEntry, ExternLocation, Externs};
use rustc_session::search_paths::{PathKind, SearchPath};
use rustc_session::utils::CanonicalizedPath;

use crate::config::Config;

#[cfg(feature = "embed-runtime")]
use std::fs;

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

pub fn inject_runtime_crate_and_deps(config: &Config, compiler_config: &mut CompilerConfig) {
    let early_dcx = EarlyDiagCtxt::new(compiler_config.opts.error_format);

    let mutest_artifacts_dir_path = if cfg!(feature = "embed-runtime") {
        &config.target_dir_root().join(MUTEST_EXTRACTED_DEPS_DIR_NAME)
    } else {
        config.mutest_search_path.as_deref().unwrap_or(Path::new(COMPILETIME_ARTIFACTS_DIR))
    };

    let mutest_deps_dir_path = if cfg!(feature = "embed-runtime") {
        mutest_artifacts_dir_path
    } else {
        match &config.mutest_search_path {
            Some(path) => &path.join("deps"),
            None => Path::new(COMPILETIME_DEPS_DIR),
        }
    };

    compiler_config.opts.search_paths.push(SearchPath::new(PathKind::Dependency, mutest_deps_dir_path.to_owned()));

    // The externs (paths to dependencies) of the `mutest_runtime` crate are baked into it at compile time.
    // These must be propagated to any crate which depends on it.
    let mut externs = BTreeMap::<String, ExternEntry>::new();
    for (key, entry) in compiler_config.opts.externs.iter() {
        externs.insert(key.clone(), entry.clone());
    }

    externs.insert("mutest_runtime".to_owned(), ExternEntry {
        location: ExternLocation::ExactPaths(BTreeSet::from([
            CanonicalizedPath::new(mutest_artifacts_dir_path.join(rlib_catalog::MUTEST_RUNTIME_RLIB_FILENAME)),
        ])),
        is_private_dep: false,
        add_prelude: false,
        nounused_dep: false,
        force: false,
    });

    for &(visible_crate_name, dep_file_name) in rlib_catalog::MUTEST_RUNTIME_PUBLIC_DEPS {
        let existing_extern = externs.insert(visible_crate_name.to_owned(), ExternEntry {
            location: ExternLocation::ExactPaths(BTreeSet::from([
                CanonicalizedPath::new(mutest_deps_dir_path.join(dep_file_name)),
            ])),
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

    compiler_config.opts.externs = Externs::new(externs);
}
