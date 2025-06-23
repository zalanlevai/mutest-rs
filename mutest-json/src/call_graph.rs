use std::time::Duration;

use serde::{Serialize, Deserialize};

/// Statistics about the crate's tests' call graph.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct CallGraphStats {
    /// Number of program functions that can have mutations introduced in them.
    pub all_mutable_fns_count: usize,
    /// Number of program functions that can be reached from entry points (i.e. tests).
    pub reachable_fns_count: usize,

    /// Total number of function calls in the crate's call graph.
    pub total_calls_count: usize,
    /// Number of virtual calls in the crate's call graph.
    /// Virtual calls are calls to trait functions through `dyn` trait objects.
    pub virtual_calls_count: usize,
    /// Number of dynamic calls in the crate's call graph.
    /// Dynamic calls are calls to opaque Rust function pointers.
    pub dynamic_calls_count: usize,
    /// Number of foreign calls in the crate's call graph.
    /// Foreign calls are calls to opaque `extern` function pointers.
    pub foreign_calls_count: usize,

    /// Depth of the crate's call graph in number of calls.
    pub call_graph_depth: usize,
}

/// Information about the crate's tests' call graph.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CallGraphInfo {
    /// Format version header.
    pub format_version: u32,

    /// Statistics about the call graph.
    pub stats: CallGraphStats,

    /// Time it took to generate the call graph.
    pub duration: Duration,
}
