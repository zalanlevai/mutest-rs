#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

use std::env;
use std::fmt::Write;
use std::fs;
use std::path::{self, Path, PathBuf};

fn fetch_rlib_deps(crate_name: &str, rlib_path: &Path, deps_dir_path: &Path) -> Vec<(String, PathBuf)> {
    use std::collections::{BTreeMap, BTreeSet};
    use std::sync::atomic::AtomicBool;

    use rustc_errors::registry::Registry;
    use rustc_interface::{Config as CompilerConfig, create_and_enter_global_ctxt, passes, run_compiler};
    use rustc_session::config::{ExternEntry, ExternLocation, Externs, Input, Options};
    use rustc_session::cstore::CrateSource;
    use rustc_session::search_paths::{PathKind, SearchPath};
    use rustc_session::utils::CanonicalizedPath;
    use rustc_span::{FileName, Symbol};

    let mut externs = BTreeMap::new();
    externs.insert(crate_name.to_owned(), ExternEntry {
        location: ExternLocation::ExactPaths(BTreeSet::from([
            CanonicalizedPath::new(rlib_path.to_owned()),
        ])),
        is_private_dep: false,
        add_prelude: true,
        nounused_dep: false,
        force: false,
    });
    let externs = Externs::new(externs);

    let compiler_config = CompilerConfig {
        opts: Options {
            search_paths: vec![
                SearchPath::new(PathKind::Dependency, deps_dir_path.to_owned()),
            ],
            externs,
            ..Default::default()
        },
        crate_cfg: vec![],
        crate_check_cfg: vec![],
        input: Input::Str {
            name: FileName::Custom("dummy.rs".to_owned()),
            input: format!("extern crate {crate_name};"),
        },
        output_dir: None,
        output_file: None,
        ice_file: None,
        file_loader: None,
        locale_resources: Default::default(),
        lint_caps: Default::default(),
        psess_created: None,
        hash_untracked_state: None,
        register_lints: None,
        override_queries: None,
        extra_symbols: Default::default(),
        make_codegen_backend: None,
        registry: Registry::new(&[]),
        using_internal_features: {
            static USING_INTERNAL_FEATURES: AtomicBool = AtomicBool::new(false);
            &USING_INTERNAL_FEATURES
        },
        expanded_args: Default::default(),
    };

    let mut dep_paths = vec![];

    run_compiler(compiler_config, |compiler| {
        let sess = &compiler.sess;

        let krate = passes::parse(sess);

        create_and_enter_global_ctxt(compiler, krate, |tcx| {
            let _ = tcx.resolver_for_lowering();

            let Some(&target_cnum) = tcx.crates(()).iter().find(|&&cnum| tcx.crate_name(cnum) == Symbol::intern(crate_name)) else {
                panic!("cannot find loaded crate for rlib");
            };

            for &cnum in tcx.crates(()) {
                if cnum == target_cnum { continue; }

                let crate_name = tcx.crate_name(cnum);

                let crate_source = tcx.used_crate_source(cnum);
                let dep_path = match &**crate_source {
                    CrateSource { rmeta: Some((rmeta_path, _)), .. } => rmeta_path.with_extension("rlib"),
                    CrateSource { rlib: Some((rlib_path, _)), .. } => rlib_path.clone(),
                    CrateSource { dylib: Some((dylib_path, _)), .. } => dylib_path.clone(),
                    _ => { continue; }
                };
                if !dep_path.starts_with(deps_dir_path) { continue; }

                dep_paths.push((crate_name.as_str().to_owned(), dep_path));
            }
        });
    });

    dep_paths
}

fn main() {
    let profile = env::var("PROFILE").unwrap();

    let build_script_out_dir_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    // Expected path format: `target/release/build/mutest-driver-HASH/out`.
    let mut build_script_out_dir_path_ancestors_iter = build_script_out_dir_path.ancestors();
    assert_eq!(Some("out"), build_script_out_dir_path_ancestors_iter.next().and_then(|p| p.file_name()).and_then(|s| s.to_str()), "unexpected `OUT_DIR` format: {}", build_script_out_dir_path.display());
    let Some(_) = build_script_out_dir_path_ancestors_iter.next() else { panic!("unexpected `OUT_DIR` format: {}", build_script_out_dir_path.display()); };
    assert_eq!(Some("build"), build_script_out_dir_path_ancestors_iter.next().and_then(|p| p.file_name()).and_then(|s| s.to_str()), "unexpected `OUT_DIR` format: {}", build_script_out_dir_path.display());
    let Some(workspace_out_dir_path) = build_script_out_dir_path_ancestors_iter.next() else { panic!("unexpected `OUT_DIR` format: {}", build_script_out_dir_path.display()) };

    // Fetch Cargo workspace metadata for visible crate names of public dependencies.
    let metadata_cmd = cargo_metadata::MetadataCommand::new();
    let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");
    // Find Cargo package in the metadata.
    let Some(mutest_runtime_package) = metadata.packages.iter().find(|package| package.name.as_ref() == "mutest-runtime") else {
        panic!("cannot find package `mutest-runtime` in Cargo metadata");
    };

    let mutest_runtime_rlib_path = workspace_out_dir_path.join("libmutest_runtime.rlib");
    let deps_dir_path = workspace_out_dir_path.join("deps");

    println!("cargo:rerun-if-changed={}", mutest_runtime_rlib_path.display());
    if !fs::exists(&mutest_runtime_rlib_path).unwrap() {
        println!("cargo::error=cannot find mutest-runtime rlib file for embedding: the crate must be built explicitly first: run `cargo build --profile={profile} -p mutest-runtime`");
        return;
    }

    // Embed absolute paths to the compiled artifacts and dependency directories,
    // which are used as fallback search paths if the dependencies are not embedded into binary
    // (i.e. for local debug builds).
    let artifacts_dir_absolute_path = path::absolute(workspace_out_dir_path).expect(&format!("cannot get absolute path for `{}`", workspace_out_dir_path.display()));
    println!("cargo:rustc-env=COMPILETIME_ARTIFACTS_DIR={}", artifacts_dir_absolute_path.display());
    let deps_dir_absolute_path = path::absolute(&deps_dir_path).expect(&format!("cannot get absolute path for `{}`", deps_dir_path.display()));
    println!("cargo:rustc-env=COMPILETIME_DEPS_DIR={}", deps_dir_absolute_path.display());

    // Fetch compiled dependencies of the crate.
    let mutest_runtime_rlib_crate_deps = fetch_rlib_deps("mutest_runtime", &mutest_runtime_rlib_path, &deps_dir_path);
    println!("mutest_runtime_rlib_crate_deps = {mutest_runtime_rlib_crate_deps:#?}");

    // Generate an rlib catalog to embed into the final binary.
    let rlib_catalog_path = build_script_out_dir_path.join("rlib_catalog.generated.rs");
    let mut rlib_catalog_content = String::new();
    // Embed the `.rlib` file into the final binary.
    writeln!(&mut rlib_catalog_content, "pub const MUTEST_RUNTIME_RLIB_FILENAME: &str = \"{}\";", mutest_runtime_rlib_path.file_name().unwrap().to_str().unwrap()).unwrap();
    writeln!(&mut rlib_catalog_content, "#[cfg(feature = \"embed-runtime\")] pub const MUTEST_RUNTIME_RLIB_DATA: &[u8] = include_bytes!(r\"{}\");", mutest_runtime_rlib_path.display()).unwrap();
    // Embed the dependency `.rlib` files into the final binary.
    writeln!(&mut rlib_catalog_content, "pub const MUTEST_RUNTIME_PUBLIC_DEPS: &[(&str, &str)] = &[").unwrap();
    for dependency in &mutest_runtime_package.dependencies {
        // NOTE: Only inject extern dependencies of mutest-runtime with the
        //       `__mutest_runtime_public_dep_` prefix, which is used both
        //       to denote an exposed dependency that needs to be injected into the generated code,
        //       and to avoid potential name collisions with the crate's own externs.
        let Some(visible_package_name) = &dependency.rename else { continue; };
        if !visible_package_name.starts_with("__mutest_runtime_public_dep_") { continue };
        // Find the compiled dependency corresponding to the dependency crate.
        let package_crate_name = dependency.name.replace("-", "_");
        let Some((_, dep_path)) = mutest_runtime_rlib_crate_deps.iter().find(|(crate_name, _)| *crate_name == package_crate_name) else { panic!("cannot find reference to package dependency `{visible_package_name}` in rlib"); };
        // Embed the mapping between the visible crate name and the underlying compiled dependency artifact.
        let visible_crate_name = visible_package_name.replace("-", "_");
        let dep_file_name = dep_path.file_name().expect(&format!("encountered rlib dependency path with no file name: `{}`", dep_path.display())).to_str().unwrap();
        writeln!(&mut rlib_catalog_content, "    (\"{}\", \"{}\"),", visible_crate_name, dep_file_name).unwrap();
    }
    writeln!(&mut rlib_catalog_content, "];").unwrap();
    writeln!(&mut rlib_catalog_content, "#[cfg(feature = \"embed-runtime\")] pub const MUTEST_RUNTIME_EXTERN_DEPS_DATA: &[(&str, &[u8])] = &[").unwrap();
    for (_dep_crate_name, dep_path) in &mutest_runtime_rlib_crate_deps {
        let dep_file_name = dep_path.file_name().expect(&format!("encountered rlib dependency path with no file name: `{}`", dep_path.display())).to_str().unwrap();
        writeln!(&mut rlib_catalog_content, "    (\"{}\", include_bytes!(r\"{}\")),", dep_file_name, dep_path.display()).unwrap();
    }
    writeln!(&mut rlib_catalog_content, "];").unwrap();
    // Write the generated rlib catalog into a Rust module source file and pass it into the final compilation.
    fs::write(&rlib_catalog_path, rlib_catalog_content).expect(&format!("cannot write file `{}`", rlib_catalog_path.display()));
    println!("cargo:rustc-env=RLIB_CATALOG={}", rlib_catalog_path.display());
}
