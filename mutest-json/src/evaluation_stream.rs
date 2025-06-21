use std::num::NonZeroU64;
use std::time::Duration;

use serde::{Serialize, Deserialize};

use crate::mutations::MutationId;

/// Whole number of nanoseconds.
/// Used in timestamps and measured execution times.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct Nanos(pub u64);

impl Nanos {
    pub fn as_duration(&self) -> Duration {
        Duration::from_nanos(self.0)
    }
}

/// Mutation evaluation stream header.
///
/// This is always the first message in the stream.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EvaluationStreamHeader {
    /// Format version header.
    pub format_version: u32,
}

/// Event corresponding to the start of a test case's execution.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TestStartEvent {
    /// Event timestamp.
    pub time: Nanos,
    /// Active mutation being evaluated.
    pub mutation_id: MutationId,
    /// Test name.
    pub test_name: String,
    /// Opaque ID of the thread the test is allocated to.
    pub thread_id: NonZeroU64,
}

/// Test result of an evaluated test case.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TestResult {
    /// Test pass.
    Ok,
    /// Test ignored; not ran.
    Ignored,
    /// Test failed.
    Failed,
    /// Test crashed.
    Crashed,
    /// Test timed out.
    TimedOut,
}

/// Event corresponding to a test result from the evaluation of a test case.
///
/// **NOTE**: For timed out lingering tests, we dispatch two [`Event::TestResult`] events;
///           one for the timeout, and one after eventual completion or termination.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TestResultEvent {
    /// Event timestamp.
    pub time: Nanos,
    /// Active mutation being evaluated.
    pub mutation_id: MutationId,
    /// Test name.
    pub test_name: String,
    /// Execution time of the test.
    /// This is [`None`] if the test was ignored and never ran.
    pub test_exec_time: Option<Nanos>,
    /// Test result.
    pub test_result: TestResult,
}

/// Mutation evaluation stream event.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "event")]
pub enum Event {
    /// Event corresponding to the start of a test's execution.
    TestStart(TestStartEvent),
    /// Event corresponding to a test result from the evaluation of a test case.
    ///
    /// **NOTE**: Multiple result events may be dispatched for the same test--mutation pair.
    ///           See [`TestResultEvent`] for more information.
    TestResult(TestResultEvent),
}
