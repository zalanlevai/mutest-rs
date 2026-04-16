use std::collections::HashMap;

use mutest_json::data_structures::IdxVec;
use mutest_json::mutations::MutationId;

#[derive(Copy, Clone, Debug)]
pub enum MutationDetection {
    Undetected,
    Detected,
    TimedOut,
    Crashed
}

impl MutationDetection {
    pub fn badge_html(&self) -> &'static str {
        match self {
            MutationDetection::Undetected => "<span class=\"status undetected\">undetected</span>",
            MutationDetection::Detected => "<span class=\"status detected\">detected</span>",
            MutationDetection::TimedOut => "<span class=\"status timed-out\">timed out</span>",
            MutationDetection::Crashed => "<span class=\"status crashed\">crashed</span>",
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TestMutationResult {
    NotRan,
    Ran(MutationDetection),
}

impl TestMutationResult {
    pub fn badge_html(&self) -> &'static str {
        match self {
            TestMutationResult::NotRan => "<span class=\"status not-ran\">not ran</span>",
            TestMutationResult::Ran(mutation_detection) => mutation_detection.badge_html(),
        }
    }
}

#[derive(Debug)]
pub struct TestRuns {
    pub mutation_detections: IdxVec<MutationId, Option<TestMutationResult>>,
}

#[derive(Debug)]
pub struct EvaluationInfo {
    pub mutation_detections: IdxVec<MutationId, MutationDetection>,
    pub test_runs: HashMap<String, TestRuns>,
}

impl EvaluationInfo {
    pub fn test_mutation_result(&self, mutation_id: MutationId, test_def_path: &str) -> Option<TestMutationResult> {
        let test_runs = self.test_runs.get(test_def_path)?;
        test_runs.mutation_detections[mutation_id]
    }
}
