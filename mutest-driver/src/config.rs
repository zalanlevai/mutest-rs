use std::path::PathBuf;

use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_interface::Config as CompilerConfig;

pub enum Mode {
    PrintMutationTargets,
    PrintMutants,
    PrintCode,
    Build,
}

pub struct Options<'op, 'm> {
    pub mode: Mode,
    pub unsafe_targeting: UnsafeTargeting,
    pub operators: Operators<'op, 'm>,
    pub mutation_depth: usize,
    pub mutant_max_mutations_count: usize,
}

pub struct Config<'op, 'm> {
    pub compiler_config: CompilerConfig,
    pub invocation_fingerprint: Option<String>,
    pub mutest_search_path: PathBuf,
    pub opts: Options<'op, 'm>,
}
