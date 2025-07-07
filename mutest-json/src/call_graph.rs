use std::collections::HashMap;
use std::time::Duration;

use serde::{Serialize, Deserialize};
use smallvec::SmallVec;

use crate::{DefId, Definition, Safety, Span};
use crate::data_structures::{Idx, IdxVec};

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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct EntryPointId(pub u32);

impl Idx for EntryPointId {
    fn as_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

/// Entry point function to the call graph.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EntryPoint {
    pub entry_point_id: EntryPointId,

    /// Definition name.
    pub name: String,
    /// Definition path.
    pub path: String,
    /// Definition span.
    pub span: Option<Span>,

    /// Calls made by the entry point, grouped by the calles it is calling,
    /// with associated instance data for each call occurance.
    pub calls: HashMap<CalleeId, SmallVec<[CallInstance; 1]>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct CalleeId(pub u32);

impl Idx for CalleeId {
    fn as_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

/// Callee function of either another callee or an entry point within the call graph.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Callee {
    pub callee_id: CalleeId,

    /// Definition this callee is a monomorphized instance of.
    pub def_id: DefId,
    /// Concrete generic arguments applied to the definition.
    pub generic_args: Vec<String>,
    /// Definition path with generic arguments applied.
    pub path_with_generic_args: String,

    /// Calls made by the callee, grouped by the calles it is calling,
    /// with associated instance data for each call occurance.
    pub calls: HashMap<CalleeId, SmallVec<[CallInstance; 1]>>,
}

/// Data associated with the instance of call from a particular caller.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct CallInstance {
    /// Span of the call's location.
    pub span: Option<Span>,
    /// Safety of the scope in which the call is in.
    pub safety: Safety,
}

/// Call graph of multiple entry points.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CallGraph {
    /// Entry point functions to the call graph.
    pub entry_points: IdxVec<EntryPointId, EntryPoint>,
    /// Callee functions of other callees and entry points within the call graph.
    pub callees: IdxVec<CalleeId, Callee>,
}

/// Information about the crate's tests' call graph.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CallGraphInfo {
    /// Format version header.
    pub format_version: u32,

    /// Statistics about the call graph.
    pub stats: CallGraphStats,
    /// Call graph.
    pub call_graph: CallGraph,

    /// Definitions referred to by the call graph.
    pub definitions: IdxVec<DefId, Definition>,

    /// Time it took to generate the call graph.
    pub duration: Duration,
}
