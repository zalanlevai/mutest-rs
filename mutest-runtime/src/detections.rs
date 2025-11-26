use std::iter;

use crate::data_structures::TestArray;
use crate::harness::{MutationTestResult, MutationTestResults};
use crate::test_runner;

pub struct MutationDetectionMatrix {
    pub(crate) inner: Vec<MutationTestResults>,
}

impl MutationDetectionMatrix {
    pub fn new(n_mutations: usize) -> Self {
        let inner = iter::repeat_with(|| Default::default()).take(n_mutations).collect::<Vec<_>>();
        Self { inner }
    }

    pub fn insert<I>(&mut self, mutation_id: u32, result: MutationTestResult, results_per_test: I)
    where
        I: IntoIterator<Item = (test::TestName, Option<MutationTestResult>)>,
    {
        self.inner[mutation_id as usize - 1].result = result;
        self.inner[mutation_id as usize - 1].results_per_test.extend(results_per_test);
    }

    pub fn iter_mutation_ids(&self) -> impl Iterator<Item = u32> {
        1..=(self.inner.len() as u32)
    }

    pub fn write_mutation_test_results(&self, mutation_id: u32, out: &mut TestArray<MutationTestResult>)
    {
        let mutation_test_results = &self.inner[mutation_id as usize - 1];
        out.fill(|test_name| mutation_test_results.results_per_test.get(test_name).copied().flatten());
    }

    pub fn iter_detections<'a>(&'a self) -> impl Iterator<Item = (u32, MutationTestResult)> + 'a {
        self.inner.iter().enumerate().map(|(mutation_idx, mutation_results)| {
            let mutation_id = mutation_idx as u32 + 1;
            (mutation_id, mutation_results.result)
        })
    }

    pub fn iter_test_detections<'a>(&'a self, test_name: &'a test::TestName) -> impl Iterator<Item = (u32, Option<MutationTestResult>)> + 'a {
        self.inner.iter().enumerate().map(|(mutation_idx, mutation_results)| {
            let mutation_id = mutation_idx as u32 + 1;
            let mutation_test_result = mutation_results.results_per_test.get(test_name).copied().flatten();
            (mutation_id, mutation_test_result)
        })
    }
}

pub fn print_mutation_detection_matrix(mutation_detection_matrix: &MutationDetectionMatrix, tests: &[test_runner::Test], warn_non_exhaustive: bool) {
    // Tests are printed in name order.
    let mut test_names = tests.iter().map(|test| test.desc.name.clone()).collect::<Vec<_>>();
    test_names.sort_unstable_by(|test_name_a, test_name_b| Ord::cmp(test_name_a.as_slice(), test_name_b.as_slice()));

    let test_name_w = test_names.iter().map(|test_name| test_name.as_slice().len()).max().unwrap_or(0);

    // Print mutation ID numbers in 10's for matrix heading, like so `1        10        20...`.
    print!("{:w$}", "", w = test_name_w + "test ".len() + 1);
    for mutation_idx in mutation_detection_matrix.iter_mutation_ids() {
        if mutation_idx == 1 {
            print!("{mutation_idx:<9}");
        } else if mutation_idx % 10 == 0 {
            print!("{mutation_idx:<10}");
        }
    }
    println!();

    // Print mutation ID numbers' last digits for matrix heading, like so `12345678901234567890123...`.
    print!("{:w$}", "", w = test_name_w + "test ".len() + 1);
    let mut mutation_id_chunks = mutation_detection_matrix.iter_mutation_ids().array_chunks::<10>();
    while let Some(_) = mutation_id_chunks.next() {
        print!("1234567890");
    }
    let last_mutation_id_chunk = mutation_id_chunks.into_remainder();
    print!("{}", &"1234567890"[..last_mutation_id_chunk.count()]);
    println!();

    // Print matrix row for overall mutation detection.
    print!("{:w$}", "total", w = test_name_w + "test ".len() + 1);
    for (_mutation_id, mutation_test_result) in mutation_detection_matrix.iter_detections() {
        match mutation_test_result {
            MutationTestResult::Undetected => print!("-"),
            MutationTestResult::Detected => print!("D"),
            MutationTestResult::Crashed => print!("C"),
            MutationTestResult::TimedOut => print!("T"),
        }
    }
    println!();

    // Print one matrix row for each test for test-mutation detections.
    for test_name in test_names {
        print!("test {:test_name_w$} ", test_name.as_slice());
        for (_mutation_id, mutation_test_result) in mutation_detection_matrix.iter_test_detections(&test_name) {
            match mutation_test_result {
                None => print!("."),
                Some(MutationTestResult::Undetected) => print!("-"),
                Some(MutationTestResult::Detected) => print!("D"),
                Some(MutationTestResult::Crashed) => print!("C"),
                Some(MutationTestResult::TimedOut) => print!("T"),
            }
        }
        println!();
    }
    println!();

    // Print legend of symbols used in the matrix.
    println!("legend: .: not ran; -: undetected; D: detected; C: crashed; T: timed out");
    println!();

    if warn_non_exhaustive {
        println!("warning: mutation detection matrix is incomplete as not all tests were evaluated, rerun with `--exhaustive`");
        println!();
    }
}
