use std::time::Duration;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TestOrdering {
    ExecTime,
    MutationDistance,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TestTimeout {
    None,
    Auto,
    Explicit(Duration),
}

pub struct Options {
    pub test_ordering: TestOrdering,
    pub test_timeout: TestTimeout,
    pub use_thread_pool: bool,
    pub report_timings: bool,
}
