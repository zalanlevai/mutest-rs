use std::time::Duration;

use serde::{Serialize, Deserialize};

use crate::Span;

/// Statistics about the crate's tests.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct TestStats {
    /// Total number of test definitions.
    pub total_tests_count: usize,
    /// Number of ignored tests.
    pub ignored_tests_count: usize,
}

/// A test case.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct Test {
    /// Name of the test. This is the full definition path to the test-defining function.
    pub name: String,
    /// Span of the test definition.
    pub span: Span,
    /// Whether the test is ignored (i.e. the `#[ignore]` attribute).
    pub ignore: bool,
}

/// Information about the crate's tests.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct TestsInfo {
    /// Format version header.
    pub format_version: u32,

    /// Statistics about the tests.
    pub stats: TestStats,
    /// All test definitions.
    pub tests: Vec<Test>,

    /// Time it took to discover the tests.
    pub duration: Duration,
}
