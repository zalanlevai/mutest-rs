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

pub use mutest_emit::codegen::mutation::GreedyMutationBatchingOrderingHeuristic;

pub enum MutationBatchingAlgorithm {
    None,
    Greedy { ordering_heuristic: Option<GreedyMutationBatchingOrderingHeuristic>, #[cfg(feature = "random")] epsilon: Option<f64> },

    #[cfg(feature = "random")]
    Random,
}

#[cfg(feature = "random")]
pub type RandomSeed = [u8; 32];

#[cfg(feature = "random")]
pub struct MutationBatchingRandomness {
    pub seed: Option<RandomSeed>,
    pub attempts: usize,
}

#[cfg(feature = "random")]
impl MutationBatchingRandomness {
    pub fn rng(&self) -> impl rand::Rng {
        use rand::prelude::*;

        match self.seed {
            Some(seed) => StdRng::from_seed(seed),
            None => StdRng::from_entropy(),
        }
    }
}

pub struct Options<'op, 'm> {
    pub mode: Mode,
    pub verbosity: u8,
    pub report_timings: bool,
    pub unsafe_targeting: UnsafeTargeting,
    pub operators: Operators<'op, 'm>,
    pub mutation_depth: usize,
    pub mutation_batching_algorithm: MutationBatchingAlgorithm,
    #[cfg(feature = "random")] pub mutation_batching_randomness: MutationBatchingRandomness,
    pub mutant_max_mutations_count: usize,
}

pub struct Config<'op, 'm> {
    pub compiler_config: CompilerConfig,
    pub invocation_fingerprint: Option<String>,
    pub mutest_search_path: PathBuf,
    pub opts: Options<'op, 'm>,
}
