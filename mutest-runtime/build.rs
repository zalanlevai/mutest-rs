#![feature(array_windows)]
#![feature(iter_intersperse)]

use std::process::Command;
use std::env;
use std::path::Path;

mod build_plan {
    #![allow(dead_code)]

    use std::collections::BTreeMap;
    use std::path::PathBuf;

    use serde::{Serialize, Deserialize, Deserializer};

    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Kind {
        Host,
        Target,
    }

    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum LibKind {
        Lib,
        Rlib,
        Dylib,
        ProcMacro,
        Other(String),
    }

    impl LibKind {
        pub fn from_str(string: &str) -> LibKind {
            match string {
                "lib" => LibKind::Lib,
                "rlib" => LibKind::Rlib,
                "dylib" => LibKind::Dylib,
                "proc-macro" => LibKind::ProcMacro,
                s => LibKind::Other(s.to_string()),
            }
        }

        pub fn crate_type(&self) -> &str {
            match *self {
                LibKind::Lib => "lib",
                LibKind::Rlib => "rlib",
                LibKind::Dylib => "dylib",
                LibKind::ProcMacro => "proc-macro",
                LibKind::Other(ref s) => s,
            }
        }

        pub fn linkable(&self) -> bool {
            match *self {
                LibKind::Lib | LibKind::Rlib | LibKind::Dylib | LibKind::ProcMacro => true,
                LibKind::Other(..) => false,
            }
        }
    }

    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum TargetKind {
        Lib(Vec<LibKind>),
        Bin,
        Test,
        Bench,
        ExampleLib(Vec<LibKind>),
        ExampleBin,
        CustomBuild,
    }

    impl<'de> Deserialize<'de> for TargetKind {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            use serde::de::Error;

            let raw = Vec::<&str>::deserialize(deserializer)?;
            Ok(match *raw {
                [] => return Err(D::Error::invalid_length(0, &"at least one target kind")),
                ["bin"] => TargetKind::Bin,
                ["example"] => TargetKind::ExampleBin,
                ["test"] => TargetKind::Test,
                ["custom-build"] => TargetKind::CustomBuild,
                ["bench"] => TargetKind::Bench,
                ref lib_kinds => TargetKind::Lib(lib_kinds.iter().cloned().map(LibKind::from_str).collect()),
            })
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Invocation {
        pub package_name: String,
        pub package_version: String,
        pub target_kind: TargetKind,
        pub kind: Option<Kind>,
        pub deps: Vec<usize>,
        pub outputs: Vec<PathBuf>,
        pub links: BTreeMap<PathBuf, PathBuf>,
        pub program: String,
        pub args: Vec<String>,
        pub env: BTreeMap<String, String>,
        pub cwd: Option<PathBuf>,
    }

    #[derive(Debug, Deserialize)]
    pub struct BuildPlan {
        pub invocations: Vec<Invocation>,
        pub inputs: Vec<PathBuf>,
    }

    impl BuildPlan {
        pub fn from_cargo_output<S: AsRef<[u8]>>(output: S) -> serde_json::Result<Self> {
            serde_json::from_slice(output.as_ref())
        }
    }
}

use build_plan::BuildPlan;

fn main() {
    let metadata_cmd = cargo_metadata::MetadataCommand::new();
    let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");

    let profile = env::var("PROFILE").unwrap();
    let pkg_name = env::var("CARGO_PKG_NAME").unwrap();

    let target_dir = metadata.target_directory.as_std_path();
    let build_plan_target_dir = target_dir.join("build-plan");

    let mut build_plan_cmd = Command::new("cargo");
    build_plan_cmd.arg("build").args(["-Z", "unstable-options"]).arg("--build-plan");
    if profile != "debug" { build_plan_cmd.args(["--profile", &profile]); }
    // NOTE: A custom target directory has to be specified for the build plan command to avoid a deadlock with the
    //       Cargo instance that invoked this build script.
    build_plan_cmd.args(["--target-dir", &build_plan_target_dir.to_string_lossy()]);

    let build_plan_out = build_plan_cmd.output().expect("failed to spawn Cargo").stdout;
    let build_plan = BuildPlan::from_cargo_output(&build_plan_out).expect("unrecognized output of `cargo --build-plan`");

    let pkg_invocation = build_plan.invocations.iter()
        .filter(|invocation| invocation.package_name == pkg_name)
        .find(|invocation| matches!(&invocation.target_kind, build_plan::TargetKind::Lib(libs) if libs.contains(&build_plan::LibKind::Rlib)))
        .expect("`cargo --build-plan` did not contain `rlib` invocation for package");

    let mutest_runtime_build_externs = pkg_invocation.args.array_windows::<2>()
        .filter_map(|[possibly_flag, possibly_value]| (possibly_flag == "--extern").then_some(possibly_value))
        .map(|extern_spec| {
            extern_spec.split_once("=")
                .map(|(prefix, path)| {
                    let path = Path::new(path).strip_prefix(&build_plan_target_dir.join(&profile)).unwrap();
                    format!("{prefix}={path}", path = path.display())
                })
                .unwrap_or(extern_spec.to_owned())
        })
        .intersperse("\x1F".to_owned())
        .collect::<String>();
    println!("cargo:rustc-env=MUTEST_RUNTIME_BUILD_EXTERNS={mutest_runtime_build_externs}");
}
