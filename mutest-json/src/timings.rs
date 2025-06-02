use std::time::Duration;

use serde::{Serialize, Deserialize};

/// Information about the time each stage of the mutation generation process took.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TimingsInfo {
    /// Total time it took to analyze the crate, generate mutations, and compile the binary.
    pub total_duration: Duration,
    /// Total time it took to analyze the crate, and generation mutations.
    pub analysis_duration: Duration,
    /// Time it took to discover the crate's tests.
    pub test_discovery_duration: Duration,
    /// Time it took to discover reachable program functions to mutate.
    pub target_analysis_duration: Duration,
    /// Time it took to sanitize code expanded from macro calls.
    pub sanitize_macro_expns_duration: Duration,
    /// Time it took to generate mutations.
    pub mutation_generation_duration: Duration,
    /// Time it took to determine conflicts between mutations.
    pub mutation_conflict_resolution_duration: Duration,
    /// Time it took to batch mutations.
    pub mutation_batching_duration: Duration,
    /// Time it took to generate the mutated program code.
    pub codegen_duration: Duration,
    /// Total time it took to write out auxiliary JSON data files.
    pub write_duration: Duration,
    /// Time to took to compile the binary.
    pub compilation_duration: Option<Duration>,
}
