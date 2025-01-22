use std::time::Duration;

pub struct PrintOptions {
    pub detection_matrix: Option<()>,
}

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
    pub verbosity: u8,
    pub report_timings: bool,
    pub print_opts: PrintOptions,
    pub exhaustive: bool,
    pub test_ordering: TestOrdering,
    pub test_timeout: TestTimeout,
    pub use_thread_pool: bool,
}
