use std::path::PathBuf;

use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_interface::Config as CompilerConfig;

#[derive(Clone, Copy)]
pub enum GraphFormat {
    Simple,
    Graphviz,
}

pub struct ConflictGraphOptions {
    pub compatibility_graph: bool,
    pub exclude_unsafe: bool,
    pub format: GraphFormat,
}

pub struct PrintOptions {
    pub print_headers: bool,
    pub mutation_targets: Option<()>,
    pub conflict_graph: Option<ConflictGraphOptions>,
    pub mutants: Option<()>,
    pub code: Option<()>,
}

impl PrintOptions {
    pub fn is_empty(&self) -> bool {
        true
            && self.mutation_targets.is_none()
            && self.conflict_graph.is_none()
            && self.mutants.is_none()
            && self.code.is_none()
    }
}

pub enum Mode {
    Print,
    Build,
}

pub use mutest_emit::codegen::mutation::GreedyMutationBatchingOrderingHeuristic;

pub enum MutationBatchingAlgorithm {
    None,
    Random,
    Greedy { ordering_heuristic: Option<GreedyMutationBatchingOrderingHeuristic>, epsilon: Option<f64> },
    SimulatedAnnealing,
}

pub type RandomSeed = [u8; 32];

pub struct MutationBatchingRandomness {
    pub seed: Option<RandomSeed>,
}

impl MutationBatchingRandomness {
    pub fn rng(&self) -> impl rand::Rng {
        use rand::prelude::*;

        match self.seed {
            Some(seed) => StdRng::from_seed(seed),
            None => StdRng::from_entropy(),
        }
    }
}

pub struct VerifyOptions {
    pub ast_lowering: bool,
}

pub struct Options<'op, 'm> {
    pub mode: Mode,
    pub verbosity: u8,
    pub report_timings: bool,
    pub print_opts: PrintOptions,
    pub unsafe_targeting: UnsafeTargeting,
    pub operators: Operators<'op, 'm>,
    pub call_graph_depth: usize,
    pub mutation_depth: usize,
    pub mutation_batching_algorithm: MutationBatchingAlgorithm,
    pub mutation_batching_randomness: MutationBatchingRandomness,
    pub mutant_max_mutations_count: usize,

    pub verify_opts: VerifyOptions,
    pub sanitize_macro_expns: bool,
}

pub struct Config<'op, 'm> {
    pub compiler_config: CompilerConfig,
    pub invocation_fingerprint: Option<String>,
    pub mutest_search_path: PathBuf,
    pub opts: Options<'op, 'm>,
}
