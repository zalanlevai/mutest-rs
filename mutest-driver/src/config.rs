use std::collections::BTreeSet;
use std::path::PathBuf;

use mutest_emit::codegen::mutation::{Operators, UnsafeTargeting};
use rustc_interface::Config as CompilerConfig;

use crate::passes::external_mutant::ExternalTargets;

/// Kinds of crates that may either be mutated, or used as a test suite (or both).
#[derive(Debug)]
pub enum CrateKind {
    /// Crate mutated against its own internal unit tests.
    MutantWithInternalTests,
    /// Crate mutated against, and driven by an external test suite.
    /// Integration tests link against this version of the mutant.
    MutantForExternalTests(ExternalTargets),
    /// External integration test crate that links against a generic mutated crate
    /// (see [`MutantForExternalTests`]).
    IntegrationTest,
}

impl CrateKind {
    pub fn produces_mutations(&self) -> bool {
        match self {
            CrateKind::MutantWithInternalTests => true,
            CrateKind::MutantForExternalTests(_) => true,
            CrateKind::IntegrationTest => false,
        }
    }

    pub fn provides_tests(&self) -> bool {
        match self {
            CrateKind::MutantWithInternalTests => true,
            CrateKind::MutantForExternalTests(_) => false,
            CrateKind::IntegrationTest => true,
        }
    }

    pub fn requires_tests(&self) -> bool {
        match self {
            CrateKind::MutantWithInternalTests => false,
            CrateKind::MutantForExternalTests(_) => true,
            CrateKind::IntegrationTest => false,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum CargoTargetKind {
    Lib,
    MainBin,
    Bin,
    Example,
    Test,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum OutputKind {
    PrintInfo,
    Metadata,
    TestBin,
}

#[derive(Copy, Clone, Debug)]
pub enum GraphFormat {
    Simple,
    Graphviz,
}

#[derive(Copy, Clone, Debug)]
pub enum CallGraphNonLocalCallView {
    Collapse,
    Expand,
}

#[derive(Clone, Debug)]
pub struct CallGraphOptions {
    pub format: GraphFormat,
    pub entry_point_filters: Vec<String>,
    pub non_local_call_view: CallGraphNonLocalCallView,
}

#[derive(Clone, Debug)]
pub struct ConflictGraphOptions {
    pub compatibility_graph: bool,
    pub exclude_unsafe: bool,
    pub format: GraphFormat,
}

#[derive(Clone, Debug)]
pub struct PrintOptions {
    pub print_headers: bool,
    pub tests: Option<()>,
    pub call_graph: Option<CallGraphOptions>,
    pub mutation_targets: Option<()>,
    pub mutations: Option<()>,
    pub conflict_graph: Option<ConflictGraphOptions>,
    pub code: Option<()>,
}

impl PrintOptions {
    pub fn is_empty(&self) -> bool {
        true
            && self.tests.is_none()
            && self.call_graph.is_none()
            && self.mutation_targets.is_none()
            && self.mutations.is_none()
            && self.conflict_graph.is_none()
            && self.code.is_none()
    }
}

#[derive(Clone, Debug)]
pub struct WriteOptions {
    pub out_dir: PathBuf,
}

pub use mutest_emit::codegen::mutation::GreedyMutationBatchingOrderingHeuristic;

#[derive(Clone, Debug)]
pub enum MutationBatchingAlgorithm {
    Random,
    Greedy { ordering_heuristic: Option<GreedyMutationBatchingOrderingHeuristic>, epsilon: Option<f64> },
    SimulatedAnnealing,
}

pub type RandomSeed = [u8; 32];

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct MutationBatchingOptions {
    pub algorithm: MutationBatchingAlgorithm,
    pub randomness: MutationBatchingRandomness,
    pub batch_max_mutations_count: usize,
}

#[derive(Clone, Debug)]
pub enum MutationParallelism {
    Batching(MutationBatchingOptions),
    DynamicScheduling,
}

#[derive(Clone, Debug)]
pub struct VerifyOptions {
    pub ast_lowering: bool,
}

#[derive(Clone, Debug)]
pub enum MutationFilter {
    /// `file:path/to/src.rs` or `file:path/to/src.rs:15` or `file:path/to/src.rs:15:17`.
    File(PathBuf, Option<(usize, Option<usize>)>),
    /// `def:path::to::function` or `def:<impl path::to::Trait for path::to::Type>::function`, using fully-qualified (without root crate) Rust item paths.
    Def(String),
}

pub struct Options<'op, 'm> {
    pub crate_kind: CrateKind,
    pub cargo_target_kind: Option<CargoTargetKind>,

    pub outputs: BTreeSet<OutputKind>,
    pub verbosity: u8,
    pub report_timings: bool,
    pub print_opts: PrintOptions,
    pub write_opts: WriteOptions,
    pub unsafe_targeting: UnsafeTargeting,
    pub operators: Operators<'op, 'm>,
    pub call_graph_depth_limit: Option<usize>,
    pub call_graph_trace_length_limit: Option<usize>,
    pub mutation_depth: usize,
    pub mutation_filters: Vec<MutationFilter>,
    pub mutation_parallelism: Option<MutationParallelism>,

    pub verify_opts: VerifyOptions,
    pub embedded: bool,
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
