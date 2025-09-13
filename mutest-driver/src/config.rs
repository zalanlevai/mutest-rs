use std::path::PathBuf;

use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_interface::Config as CompilerConfig;

/// Kinds of crates that may either be mutated, or used as a test suite (or both).
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum CrateKind {
    /// Crate mutated against its own internal unit tests.
    MutantWithInternalTests,
    /// Crate mutated against its public interface, driven by an external test suite.
    /// Integration tests link against this version of the mutant.
    MutantForExternalTests,
    /// External integration test crate that links against a generic mutated crate
    /// (see [MutantForExternalTests]).
    IntegrationTest,
}

impl CrateKind {
    pub fn produces_mutations(&self) -> bool {
        match self {
            CrateKind::MutantWithInternalTests => true,
            CrateKind::MutantForExternalTests => true,
            CrateKind::IntegrationTest => false,
        }
    }

    pub fn provides_tests(&self) -> bool {
        match self {
            CrateKind::MutantWithInternalTests => true,
            CrateKind::MutantForExternalTests => false,
            CrateKind::IntegrationTest => true,
        }
    }

    pub fn requires_tests(&self) -> bool {
        match self {
            CrateKind::MutantWithInternalTests => false,
            CrateKind::MutantForExternalTests => true,
            CrateKind::IntegrationTest => false,
        }
    }
}

#[derive(Clone, Copy)]
pub enum GraphFormat {
    Simple,
    Graphviz,
}

#[derive(Clone, Copy)]
pub enum CallGraphNonLocalCallView {
    Collapse,
    Expand,
}

pub struct CallGraphOptions {
    pub format: GraphFormat,
    pub entry_point_filters: Vec<String>,
    pub non_local_call_view: CallGraphNonLocalCallView,
}

pub struct ConflictGraphOptions {
    pub compatibility_graph: bool,
    pub exclude_unsafe: bool,
    pub format: GraphFormat,
}

pub struct PrintOptions {
    pub print_headers: bool,
    pub tests: Option<()>,
    pub mutation_targets: Option<()>,
    pub call_graph: Option<CallGraphOptions>,
    pub conflict_graph: Option<ConflictGraphOptions>,
    pub mutations: Option<()>,
    pub code: Option<()>,
}

impl PrintOptions {
    pub fn is_empty(&self) -> bool {
        true
            && self.tests.is_none()
            && self.mutation_targets.is_none()
            && self.call_graph.is_none()
            && self.conflict_graph.is_none()
            && self.mutations.is_none()
            && self.code.is_none()
    }
}

pub struct WriteOptions {
    pub out_dir: PathBuf,
}

pub enum Mode {
    Print,
    Build,
}

pub use mutest_emit::codegen::mutation::GreedyMutationBatchingOrderingHeuristic;

pub enum MutationBatchingAlgorithm {
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
            None => StdRng::from_os_rng(),
        }
    }
}

pub struct MutationBatchingOptions {
    pub algorithm: MutationBatchingAlgorithm,
    pub randomness: MutationBatchingRandomness,
    pub batch_max_mutations_count: usize,
}

pub enum MutationParallelism {
    Batching(MutationBatchingOptions),
}

pub struct VerifyOptions {
    pub ast_lowering: bool,
}

pub struct Options<'op, 'm> {
    pub crate_kind: CrateKind,

    pub mode: Mode,
    pub verbosity: u8,
    pub report_timings: bool,
    pub print_opts: PrintOptions,
    pub unsafe_targeting: UnsafeTargeting,
    pub operators: Operators<'op, 'm>,
    pub call_graph_depth_limit: Option<usize>,
    pub call_graph_trace_length_limit: Option<usize>,
    pub mutation_depth: usize,
    pub mutation_parallelism: Option<MutationParallelism>,

    pub write_opts: Option<WriteOptions>,
    pub verify_opts: VerifyOptions,
    pub sanitize_macro_expns: bool,
}

pub struct Config<'op, 'm> {
    pub compiler_config: CompilerConfig,
    pub invocation_fingerprint: Option<String>,
    pub mutest_target_dir_root: Option<PathBuf>,
    pub mutest_search_path: Option<PathBuf>,
    pub opts: Options<'op, 'm>,
}

impl<'op, 'm> Config<'op, 'm> {
    pub fn target_dir_root(&self) -> PathBuf {
        self.mutest_target_dir_root.clone().unwrap_or(self.compiler_config.output_dir.clone().unwrap_or_default())
    }
}
