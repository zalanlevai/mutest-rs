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

#[derive(Debug)]
pub struct EvaluationInfo {
    pub mutation_detections: IdxVec<MutationId, MutationDetection>,
}
