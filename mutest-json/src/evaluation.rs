use std::collections::HashMap;
use std::fmt::Write;
use std::time::Duration;

use serde::{Serialize, Deserialize};
use smallvec::SmallVec;

use crate::data_structures::{Idx, IdxVec};
use crate::mutations::MutationId;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct RuntimeTestId(pub u32);

impl Idx for RuntimeTestId {
    fn as_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

/// A test case.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct RuntimeTest {
    pub runtime_test_id: RuntimeTestId,

    /// Name of the test. This is the full definition path to the test-defining function.
    pub name: String,

    /// Execution time of the test without any mutations applied.
    pub unmutated_exec_time: Option<Duration>,
    /// Time after which the test's execution is considered to have timed out.
    pub timeout: Option<Duration>,
}

/// Statistics about the detection of mutations.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct MutationDetectionStats {
    /// Mutation score of constituent mutations, ranging between 0.0 (0%) and 1.0 (100%).
    /// This is [`None`] if there are no constituent mutations.
    pub mutation_score: Option<f64>,
    /// Total number of constituent mutations.
    pub total_mutations_count: usize,
    /// Number of detected constituent mutations.
    /// This includes detections through timeouts and crashes.
    pub detected_mutations_count: usize,
    /// Number of timed out constituent mutations.
    pub timed_out_mutations_count: usize,
    /// Number of crashed constituent mutations.
    pub crashed_mutations_count: usize,
    /// Number of undetected constituent mutations.
    pub undetected_mutations_count: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum MutationDetection {
    NotRun,
    Detected,
    Undetected,
    TimedOut,
    Crashed,
}

#[derive(Clone, Debug)]
pub struct MutationDetections(pub IdxVec<MutationId, MutationDetection>);

impl Serialize for MutationDetections {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;

        let mut string_encoded_detections = String::with_capacity(self.0.len());
        for detection in &self.0 {
            write!(&mut string_encoded_detections, "{}", match detection {
                MutationDetection::NotRun => ".",
                MutationDetection::Detected => "D",
                MutationDetection::Undetected => "-",
                MutationDetection::TimedOut => "T",
                MutationDetection::Crashed => "C",
            }).map_err(|_| S::Error::custom("cannot write detection string"))?;
        }

        serializer.serialize_str(&string_encoded_detections)
    }
}

impl<'de> Deserialize<'de> for MutationDetections {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let string_encoded_detections = <&str>::deserialize(deserializer)?;

        let mut decoded_detections = IdxVec::with_capacity(string_encoded_detections.len());
        for (char_offset, char_encoded_detection) in string_encoded_detections.chars().enumerate() {
            let decoded_detection = match char_encoded_detection {
                '.' => MutationDetection::NotRun,
                'D' => MutationDetection::Detected,
                '-' => MutationDetection::Undetected,
                'T' => MutationDetection::TimedOut,
                'C' => MutationDetection::Crashed,
                _ => { return Err(D::Error::custom(format!("unknown detection character code at offset {}", char_offset))); }
            };
            decoded_detections.push(decoded_detection);
        }

        Ok(MutationDetections(decoded_detections))
    }
}

/// Test--mutation detection matrix,
/// depicting mutation detections between individual test--mutation pairs.
///
/// The detections are encoded in a simple string format,
/// with each mutation's detection being depicted by a corresponding ASCII character,
/// similar to how they are printed to stdout.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MutationDetectionMatrix {
    /// Overall detection of mutations across all evaluated tests.
    pub overall_detections: MutationDetections,
    /// Detections of mutations by individual tests.
    pub test_detections: IdxVec<RuntimeTestId, MutationDetections>,
}

/// Results of a mutation run.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MutationRun {
    /// Detection statistics about all mutations.
    pub all_mutations_detection_stats: MutationDetectionStats,
    /// Detection statistics about safe mutations.
    pub safe_mutations_detection_stats: MutationDetectionStats,
    /// Detection statistics about unsafe mutations.
    pub unsafe_mutations_detection_stats: MutationDetectionStats,
    /// Individual detection statistics about the mutations generated by each mutation operator.
    pub per_op_mutation_detection_stats: HashMap<String, MutationDetectionStats>,

    /// Test--mutation detection matrix.
    pub mutation_detection_matrix: MutationDetectionMatrix,

    /// Time it took to perform the mutation run.
    pub duration: Duration,
}

/// Test--mutation detection flakiness matrix,
/// depicting flakiness in mutation detections between individual test--mutation pairs.
///
/// The detection flakiness is encoded in a simple string format,
/// with each mutation's detection flakiness being depicted by a corresponding ASCII character,
/// similar to how they are printed to stdout.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MutationFlakinessMatrix {
    /// Overall detection flakiness of mutations across all evaluated tests.
    pub overall_detection_flakiness: String,
    /// Detection flakiness of mutations by individual tests.
    pub test_detection_flakiness: IdxVec<RuntimeTestId, String>,
}

/// Analysis of flakiness in test--mutation detections based on repeated mutation runs.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MutationFlakinessAnalysis {
    /// Test--mutation detection flakiness matrix.
    pub mutation_flakiness_matrix: MutationFlakinessMatrix,
    /// Total time it took to run every mutation run iteration.
    pub duration: Duration,
}

/// Information about the mutation evaluation.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EvaluationInfo {
    /// Format version header.
    pub format_version: u32,

    /// Mutation runs.
    pub mutation_runs: SmallVec<[MutationRun; 1]>,

    /// Analysis of flakiness in test--mutation detections.
    pub flakiness_analysis: Option<MutationFlakinessAnalysis>,

    /// Test cases.
    pub tests: IdxVec<RuntimeTestId, RuntimeTest>,
    /// Time it took to profile the unmutated tests.
    pub test_profiling_duration: Duration,

    /// Total time it took to evaluate the mutations.
    pub duration: Duration,
}
