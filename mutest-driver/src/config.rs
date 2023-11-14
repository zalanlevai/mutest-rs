use std::path::PathBuf;

use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_interface::Config as CompilerConfig;

#[derive(Clone, Copy)]
pub enum GraphFormat {
    Simple,
    Graphviz,
}

pub enum Mode {
    PrintMutationTargets,
    PrintConflictGraph { compatibility_graph: bool, exclude_unsafe: bool, format: GraphFormat },
    PrintMutants,
    PrintCode,
    Build,
}

pub enum MutationBatchingAlgorithm {
    Greedy,

    #[cfg(feature = "random")]
    Random { seed: Option<[u8; 32]>, attempts: usize },
}

pub struct Options<'op, 'm> {
    pub mode: Mode,
    pub verbosity: u8,
    pub report_timings: bool,
    pub unsafe_targeting: UnsafeTargeting,
    pub operators: Operators<'op, 'm>,
    pub mutation_depth: usize,
    pub mutation_batching_algorithm: MutationBatchingAlgorithm,
    pub mutant_max_mutations_count: usize,
}

pub struct Config<'op, 'm> {
    pub compiler_config: CompilerConfig,
    pub invocation_fingerprint: Option<String>,
    pub mutest_search_path: PathBuf,
    pub opts: Options<'op, 'm>,
}
