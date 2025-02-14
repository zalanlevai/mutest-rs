use std::collections::{HashMap, HashSet};

use crate::detections::MutationDetectionMatrix;
use crate::harness::MutationTestResult;
use crate::test_runner;

pub fn compute_flakiness<I>(mutation_test_results: I) -> bool
where
    I: IntoIterator<Item = MutationTestResult>,
{
    let mutation_test_results = mutation_test_results.into_iter().collect::<HashSet<_>>();
    // A mutation detection is flaky iff across various runs it is both undetected at times, and detected at other times (with any reason).
    // NOTE: We currently do not differentiate between detection reasons (i.e. test fails vs. crashes vs. timeouts) when considering flakiness.
    mutation_test_results.contains(&MutationTestResult::Undetected) && mutation_test_results.len() > 1
}

pub struct MutationFlakiness {
    pub detection_flakiness: bool,
    pub flakiness_per_test: HashMap<test::TestName, Option<bool>>,
}

pub struct MutationFlakinessMatrix {
    pub(crate) inner: Vec<MutationFlakiness>,
}

impl MutationFlakinessMatrix {
    pub fn build(n_mutations: usize, mutation_detection_matrices: &[&MutationDetectionMatrix]) -> Self {
        let inner = (0..n_mutations)
            .map(|mutation_idx| {
                let detections = mutation_detection_matrices.iter()
                    .map(|mutation_detection_matrix| mutation_detection_matrix.inner[mutation_idx].result);
                let detection_flakiness = compute_flakiness(detections);

                let test_names = mutation_detection_matrices.iter()
                    .flat_map(|mutation_detection_matrix| mutation_detection_matrix.inner[mutation_idx].results_per_test.keys())
                    .collect::<HashSet<_>>();
                let flakiness_per_test = test_names.into_iter()
                    .map(|test_name| {
                        // Mutation test results of the same test across the various runs, ignoring runs where the test was not run.
                        let detections = mutation_detection_matrices.iter()
                            .map(|mutation_detection_matrix| mutation_detection_matrix.inner[mutation_idx].results_per_test.get(test_name).copied().flatten())
                            .flatten();
                        (test_name.clone(), Some(compute_flakiness(detections)))
                    })
                    .collect::<HashMap<_, _>>();

                MutationFlakiness { detection_flakiness, flakiness_per_test }
            })
            .collect::<Vec<_>>();

        Self { inner }
    }

    pub fn iter_mutation_ids(&self) -> impl Iterator<Item = u32> {
        1..=(self.inner.len() as u32)
    }

    pub fn iter_detection_flakes<'a>(&'a self) -> impl Iterator<Item = (u32, bool)> + 'a {
        self.inner.iter().enumerate().map(|(mutation_idx, mutation_flakiness)| {
            let mutation_id = mutation_idx as u32 + 1;
            (mutation_id, mutation_flakiness.detection_flakiness)
        })
    }

    pub fn iter_test_flakes<'a>(&'a self, test_name: &'a test::TestName) -> impl Iterator<Item = (u32, Option<bool>)> + 'a {
        self.inner.iter().enumerate().map(|(mutation_idx, mutation_flakiness)| {
            let mutation_id = mutation_idx as u32 + 1;
            let mutation_test_flakiness = mutation_flakiness.flakiness_per_test.get(test_name).copied().flatten();
            (mutation_id, mutation_test_flakiness)
        })
    }
}

pub fn print_mutation_flakiness_matrix(mutation_flakiness_matrix: &MutationFlakinessMatrix, tests: &[test_runner::Test]) {
    // Tests are printed in name order.
    let mut test_names = tests.iter().map(|test| test.desc.name.clone()).collect::<Vec<_>>();
    test_names.sort_unstable_by(|test_name_a, test_name_b| Ord::cmp(test_name_a.as_slice(), test_name_b.as_slice()));

    let test_name_w = test_names.iter().map(|test_name| test_name.as_slice().len()).max().unwrap_or(0);

    // Print mutation ID numbers in 10's for matrix heading, like so `1        10        20...`.
    print!("{:w$}", "", w = test_name_w + "test ".len() + 1);
    for mutation_idx in mutation_flakiness_matrix.iter_mutation_ids() {
        if mutation_idx == 1 {
            print!("{mutation_idx:<9}");
        } else if mutation_idx % 10 == 0 {
            print!("{mutation_idx:<10}");
        }
    }
    println!();

    // Print mutation ID numbers' last digits for matrix heading, like so `12345678901234567890123...`.
    print!("{:w$}", "", w = test_name_w + "test ".len() + 1);
    let mut mutation_id_chunks = mutation_flakiness_matrix.iter_mutation_ids().array_chunks::<10>();
    while let Some(_) = mutation_id_chunks.next() {
        print!("1234567890");
    }
    if let Some(last_mutation_id_chunk) = mutation_id_chunks.into_remainder() {
        print!("{}", &"1234567890"[..last_mutation_id_chunk.count()]);
    }
    println!();

    // Print matrix row for overall mutation flakiness.
    print!("{:w$}", "total", w = test_name_w + "test ".len() + 1);
    for (_mutation_id, detection_flakiness) in mutation_flakiness_matrix.iter_detection_flakes() {
        match detection_flakiness {
            false => print!("-"),
            true => print!("F"),
        }
    }
    println!();

    // Print one matrix row for each test for test-mutation flakiness.
    for test_name in test_names {
        print!("test {:test_name_w$} ", test_name.as_slice());
        for (_mutation_id, mutation_test_flakiness) in mutation_flakiness_matrix.iter_test_flakes(&test_name) {
            match mutation_test_flakiness {
                None => print!("."),
                Some(false) => print!("-"),
                Some(true) => print!("F"),
            }
        }
        println!();
    }
    println!();

    // Print legend of symbols used in the matrix.
    println!("legend: .: not ran; -: not flaky; F: flaky");
    println!();
}

pub fn print_mutation_flakiness_epilogue(mutation_flakiness_matrix: &MutationFlakinessMatrix, tests: &[test_runner::Test]) {
    let total_mutations_count = mutation_flakiness_matrix.iter_mutation_ids().count();
    let total_tests_count = tests.len();
    let total_test_mutation_pairs_count = total_mutations_count * total_tests_count;

    let mut ran_test_mutation_pairs_count = 0;
    let mut flaky_test_mutation_pairs_count = 0;

    for test in tests {
        for (_mutation_id, mutation_test_flakiness) in mutation_flakiness_matrix.iter_test_flakes(&test.desc.name) {
            if let Some(mutation_test_flakiness) = mutation_test_flakiness {
                ran_test_mutation_pairs_count += 1;

                match mutation_test_flakiness {
                    false => {}
                    true => flaky_test_mutation_pairs_count += 1,
                }
            }
        }
    }

    println!("flakiness: {score}. {flaky} flaky; {not_flaky} not flaky; {not_ran} not ran; {total} total",
        score = match total_test_mutation_pairs_count {
            0 => "none".to_owned(),
            _ => format!("{:.2}%", flaky_test_mutation_pairs_count as f64 / total_test_mutation_pairs_count as f64 * 100_f64),
        },
        flaky = flaky_test_mutation_pairs_count,
        not_flaky = ran_test_mutation_pairs_count - flaky_test_mutation_pairs_count,
        not_ran = total_test_mutation_pairs_count - ran_test_mutation_pairs_count,
        total = total_test_mutation_pairs_count,
    );
}

#[cfg(test)]
mod tests {
    use crate::harness::MutationTestResult;

    use super::compute_flakiness;

    #[test]
    fn test_no_tests_not_flaky() {
        assert_eq!(compute_flakiness([]), false);
    }

    #[test]
    fn test_always_detected_tests_not_flaky() {
        assert_eq!(
            compute_flakiness([MutationTestResult::Detected, MutationTestResult::Detected, MutationTestResult::Detected]),
            false,
        );
    }

    #[test]
    fn test_always_undetected_tests_not_flaky() {
        assert_eq!(
            compute_flakiness([MutationTestResult::Undetected, MutationTestResult::Undetected, MutationTestResult::Undetected]),
            false,
        );
    }

    #[test]
    fn test_flaky_tests_flaky() {
        assert_eq!(
            compute_flakiness([MutationTestResult::Detected, MutationTestResult::Detected, MutationTestResult::Undetected]),
            true,
        );
        assert_eq!(
            compute_flakiness([MutationTestResult::Undetected, MutationTestResult::Crashed, MutationTestResult::Undetected]),
            true,
        );
        assert_eq!(
            compute_flakiness([MutationTestResult::Undetected, MutationTestResult::Undetected, MutationTestResult::TimedOut]),
            true,
        );
    }

    #[test]
    fn test_ignore_detection_reason_flakiness() {
        assert_eq!(
            compute_flakiness([MutationTestResult::Detected, MutationTestResult::TimedOut, MutationTestResult::Crashed]),
            false,
        );
    }
}
