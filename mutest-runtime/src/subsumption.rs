use std::iter;

use crate::data_structures::TestArray;
use crate::detections::MutationDetectionMatrix;
use crate::harness::MutationTestResult;
use crate::metadata::MutationMeta;
use crate::test_runner;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum MutationSubsumption {
    No,
    Subsumed,
    Subsumes,
    Indistinguishable,
}

impl MutationSubsumption {
    pub fn subsumed(&self) -> bool {
        matches!(self, MutationSubsumption::Subsumed | MutationSubsumption::Indistinguishable)
    }

    pub fn subsumes(&self) -> bool {
        matches!(self, MutationSubsumption::Subsumes | MutationSubsumption::Indistinguishable)
    }

    pub fn indistinguishable(&self) -> bool {
        matches!(self, MutationSubsumption::Indistinguishable)
    }

    pub fn reverse(&self) -> MutationSubsumption {
        match self {
            MutationSubsumption::No => MutationSubsumption::No,
            MutationSubsumption::Subsumed => MutationSubsumption::Subsumes,
            MutationSubsumption::Subsumes => MutationSubsumption::Subsumed,
            MutationSubsumption::Indistinguishable => MutationSubsumption::Indistinguishable,
        }
    }
}

pub fn compute_mutation_subsumption<I1, I2>(mutation_test_results: I1, other_mutation_test_results: I2) -> MutationSubsumption
where
    I1: IntoIterator<Item = Option<MutationTestResult>>,
    I2: IntoIterator<Item = Option<MutationTestResult>>,
{
    let mut indistinguishable = true;
    let mut has_detecting_test = false;
    let mut other_has_detecting_test = false;
    let mut has_subsumed_test = false;
    let mut has_subsuming_test = false;

    for (mutation_test_result, other_mutation_test_result) in iter::zip(mutation_test_results, other_mutation_test_results) {
        let mutation_test_result = mutation_test_result.unwrap_or(MutationTestResult::Undetected);
        let other_mutation_test_result = other_mutation_test_result.unwrap_or(MutationTestResult::Undetected);

        if mutation_test_result != MutationTestResult::Undetected { has_detecting_test = true; }
        if other_mutation_test_result != MutationTestResult::Undetected { other_has_detecting_test = true; }

        match (mutation_test_result, other_mutation_test_result) {
            // Distinguishable results, but does not break subsumption relation.
            (_, MutationTestResult::Undetected) if mutation_test_result != MutationTestResult::Undetected => {
                indistinguishable = false;
                has_subsuming_test = true;
            }
            // Breaks subsumption relation; detections are not a subset.
            (MutationTestResult::Undetected, _) if other_mutation_test_result != MutationTestResult::Undetected => {
                indistinguishable = false;
                has_subsumed_test = true;
            }
            // Indistinguishable results.
            _ => {}
        }
    }

    if !has_detecting_test || !other_has_detecting_test { return MutationSubsumption::No; }
    if indistinguishable { return MutationSubsumption::Indistinguishable; }

    match (has_subsuming_test, has_subsumed_test) {
        (true, true) | (false, false) =>  MutationSubsumption::No,
        (true, false) => MutationSubsumption::Subsumes,
        (false, true) => MutationSubsumption::Subsumed,
    }
}

pub struct MutationSubsumptionMatrix {
    n_mutations: u32,
    inner: Box<[MutationSubsumption]>,
}

impl MutationSubsumptionMatrix {
    pub fn build(mutation_detection_matrix: &MutationDetectionMatrix, tests: &[test_runner::Test]) -> Self {
        let test_index = tests.iter().map(|test| &test.desc.name).collect::<Vec<_>>();
        let mut mutation_test_results = TestArray::<MutationTestResult>::new(&test_index);
        let mut other_mutation_test_results = TestArray::<MutationTestResult>::new(&test_index);

        let n_mutations = mutation_detection_matrix.inner.len() as u32;
        let mut inner = iter::repeat(MutationSubsumption::No).take(n_mutations as usize * n_mutations as usize).collect::<Box<[_]>>();

        let mut mutation_ids = 1..=n_mutations;
        while let Some(mutation_id) = mutation_ids.next() {
            mutation_detection_matrix.write_mutation_test_results(mutation_id, &mut mutation_test_results);

            for other_mutation_id in mutation_ids.clone() {
                mutation_detection_matrix.write_mutation_test_results(other_mutation_id, &mut other_mutation_test_results);

                let subsumption = compute_mutation_subsumption(mutation_test_results.iter_data().copied(), other_mutation_test_results.iter_data().copied());

                inner[n_mutations as usize * (mutation_id as usize - 1) + (other_mutation_id as usize - 1)] = subsumption;
                inner[n_mutations as usize * (other_mutation_id as usize - 1) + (mutation_id as usize - 1)] = subsumption.reverse();
            }
        }

        Self { n_mutations, inner }
    }

    pub fn iter_mutation_ids(&self) -> impl Iterator<Item = u32> {
        1..=(self.n_mutations as u32)
    }

    pub fn subsumptions_of(&self, mutation_id: u32) -> &[MutationSubsumption] {
        if mutation_id > self.n_mutations {
            panic!("index out of bounds: the mutation id is {mutation_id} but the matrix holds data for {} mutations", self.n_mutations);
        }

        let offset = self.n_mutations as usize * (mutation_id as usize - 1);
        &self.inner[offset..(offset + self.n_mutations as usize)]
    }

    pub fn subsumption_between(&self, mutation_id: u32, other_mutation_id: u32) -> Option<MutationSubsumption> {
        self.inner.get(self.n_mutations as usize * (mutation_id as usize - 1) + (other_mutation_id as usize - 1)).copied()
    }
}

pub fn print_mutation_subsumption_matrix(mutation_subsumption_matrix: &MutationSubsumptionMatrix, mutations: &[&MutationMeta], warn_non_exhaustive: bool) {
    let total_mutations_count = mutations.len();
    let mutation_id_w = total_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1;

    // Print mutation ID numbers in 10's for matrix heading, like so `1        10        20...`.
    print!("{:w$}", "", w = mutation_id_w + 1);
    for mutation_idx in mutation_subsumption_matrix.iter_mutation_ids() {
        if mutation_idx == 1 {
            print!("{mutation_idx:<9}");
        } else if mutation_idx % 10 == 0 {
            print!("{mutation_idx:<10}");
        }
    }
    println!();

    // Print mutation ID numbers' last digits for matrix heading, like so `12345678901234567890123...`.
    print!("{:w$}", "", w = mutation_id_w + 1);
    let mut mutation_id_chunks = mutation_subsumption_matrix.iter_mutation_ids().array_chunks::<10>();
    while let Some(_) = mutation_id_chunks.next() {
        print!("1234567890");
    }
    if let Some(last_mutation_id_chunk) = mutation_id_chunks.into_remainder() {
        print!("{}", &"1234567890"[..last_mutation_id_chunk.count()]);
    }
    // Print additional headers.
    print!(" {:>mutation_id_w$}", "<=");
    print!(" {:>mutation_id_w$}", ">=");
    print!(" {:>mutation_id_w$}", "=");
    println!();

    // Print one matrix row for each mutation for mutation-mutation subsumptions.
    for mutation_id in mutation_subsumption_matrix.iter_mutation_ids() {
        print!("{:>mutation_id_w$} ", mutation_id);

        let mut subsumes_count: usize = 0;
        let mut subsumed_count: usize = 0;
        let mut indistinguishable_count: usize = 0;

        for (other_mutation_idx, subsumption) in mutation_subsumption_matrix.subsumptions_of(mutation_id).iter().enumerate() {
            let other_mutation_id = other_mutation_idx as u32 + 1;
            if other_mutation_id == mutation_id {
                print!(" ");
                continue;
            }

            match subsumption {
                MutationSubsumption::No => print!("."),
                MutationSubsumption::Subsumed => print!("<"),
                MutationSubsumption::Subsumes => print!(">"),
                MutationSubsumption::Indistinguishable => print!("="),
            }

            if subsumption.subsumes() { subsumes_count += 1; }
            if subsumption.subsumed() { subsumed_count += 1; }
            if subsumption.indistinguishable() { indistinguishable_count += 1; }
        }

        print!(" {:>mutation_id_w$}", subsumed_count);
        print!(" {:>mutation_id_w$}", subsumes_count);
        print!(" {:>mutation_id_w$}", indistinguishable_count);

        let mutation = mutations.iter().find(|mutation| mutation.id == mutation_id).unwrap();
        print!(" {}", mutation.op_name);

        println!();
    }
    println!();

    // Print legend of symbols used in the matrix.
    println!("legend: .: none; <: subsumed; >: subsumes; =: indistinguishable");
    println!();

    if warn_non_exhaustive {
        println!("warning: mutation subsumption matrix may be inaccurate as not all tests were evaluated, rerun with `--exhaustive`");
        println!();
    }
}

#[cfg(test)]
mod tests {
    use crate::harness::MutationTestResult;

    use super::{MutationSubsumption, compute_mutation_subsumption};

    #[test]
    fn test_either_result_sequence_empty_no_subsumption_relation() {
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), None],
                [Some(MutationTestResult::Undetected), None, Some(MutationTestResult::Undetected)],
            ),
            MutationSubsumption::No,
        );
        assert_eq!(
            compute_mutation_subsumption(
                [None, Some(MutationTestResult::Undetected), None],
                [Some(MutationTestResult::Detected), None, Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::No,
        );
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Undetected), None],
                [None, Some(MutationTestResult::Undetected), None],
            ),
            MutationSubsumption::No,
        );
    }

    #[test]
    fn test_no_common_detections_no_subsumption_relation() {
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected)],
                [Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::No,
        );
    }

    #[test]
    fn test_have_common_detections_both_with_unique_detections_no_subsumption_relation() {
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Detected), Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected)],
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::No,
        );
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected)],
                [Some(MutationTestResult::Detected), Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected)],
            ),
            MutationSubsumption::No,
        );
    }

    #[test]
    fn test_have_common_detecions_and_this_has_additional_detections_subsumes() {
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Detected), Some(MutationTestResult::Detected), None],
                [None, Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected)],
            ),
            MutationSubsumption::Subsumes,
        );
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), Some(MutationTestResult::Detected)],
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::Subsumes,
        );
    }

    #[test]
    fn test_have_common_detections_and_other_has_additonal_detecions_subsumed() {
        assert_eq!(
            compute_mutation_subsumption(
                [None, Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected)],
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::Subsumed,
        );
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected), Some(MutationTestResult::Undetected)],
                [Some(MutationTestResult::Detected), None, Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::Subsumed,
        );
    }

    #[test]
    fn test_all_common_detections_indistinguishable() {
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), Some(MutationTestResult::Detected)],
                [Some(MutationTestResult::Undetected), Some(MutationTestResult::Detected), Some(MutationTestResult::Detected)],
            ),
            MutationSubsumption::Indistinguishable,
        );
    }

    #[test]
    fn test_ignore_detection_reason_subsumption() {
        assert_eq!(
            compute_mutation_subsumption(
                [Some(MutationTestResult::Crashed), Some(MutationTestResult::Detected), Some(MutationTestResult::TimedOut), None],
                [Some(MutationTestResult::TimedOut), Some(MutationTestResult::Crashed), Some(MutationTestResult::Detected), Some(MutationTestResult::Undetected)],
            ),
            MutationSubsumption::Indistinguishable,
        );
    }
}
