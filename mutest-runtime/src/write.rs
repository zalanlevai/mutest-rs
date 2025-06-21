use std::collections::HashMap;
use std::fs;
use std::io::{BufWriter, Write};
use std::num::NonZeroU64;
use std::ops::DerefMut;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::thread::ThreadId;
use std::time::{Duration, Instant};

use crate::config::WriteOptions;
use crate::harness::{MutationAnalysisResults, MutationTestResult};
use crate::flakiness::MutationFlakinessMatrix;
use crate::metadata::MutationMeta;
use crate::test_runner;

#[derive(Clone)]
pub struct EvaluationStreamWriter {
    buffered_file: Arc<Mutex<BufWriter<fs::File>>>,
    t_start: Instant,
}

impl EvaluationStreamWriter {
    pub fn new(path: &Path, t_start: Instant) -> Self {
        let file = fs::File::create(path).expect("cannot create stream file");
        let buffered_file = Arc::new(Mutex::new(BufWriter::new(file)));
        let eval_stream_writer = Self { buffered_file, t_start };

        eval_stream_writer.write_event(&mutest_json::evaluation_stream::EvaluationStreamHeader {
            format_version: mutest_json::FORMAT_VERSION,
        });

        eval_stream_writer
    }

    #[inline]
    pub fn timestamp(&self) -> Duration {
        self.t_start.elapsed()
    }

    pub fn write_event<T: serde::Serialize>(&self, data: &T) {
        let mut eval_stream_file = self.buffered_file.lock().unwrap();
        serde_json::to_writer(eval_stream_file.deref_mut(), &data).expect("cannot write to stream file");
        writeln!(eval_stream_file.deref_mut(), "").expect("cannot write to stream file");
    }

    pub fn write_test_start(&self, mutation: &MutationMeta, test_desc: &test::TestDesc, thread_id: Option<ThreadId>) {
        let t = self.timestamp();

        self.write_event(&mutest_json::evaluation_stream::Event::TestStart(mutest_json::evaluation_stream::TestStartEvent {
            time: mutest_json::evaluation_stream::Nanos(t.as_nanos().try_into().expect("cannot fit timestamp in u64")),
            mutation_id: mutest_json::mutations::MutationId(mutation.id),
            test_name: test_desc.name.as_slice().to_owned(),
            thread_id: thread_id.map_or(const { NonZeroU64::new(1).unwrap() }, |thread_id| thread_id.as_u64())
        }))
    }

    pub fn write_test_result(&self, mutation: &MutationMeta, test: &test_runner::CompletedTest) {
        let t = self.timestamp();

        self.write_event(&mutest_json::evaluation_stream::Event::TestResult(mutest_json::evaluation_stream::TestResultEvent {
            time: mutest_json::evaluation_stream::Nanos(t.as_nanos().try_into().expect("cannot fit timestamp in u64")),
            mutation_id: mutest_json::mutations::MutationId(mutation.id),
            test_name: test.desc.name.as_slice().to_owned(),
            test_exec_time: test.exec_time.map(|test_exec_time| mutest_json::evaluation_stream::Nanos(test_exec_time.as_nanos().try_into().expect("cannot fit exec time in u64"))),
            test_result: match test.result {
                test_runner::TestResult::Ok => mutest_json::evaluation_stream::TestResult::Ok,
                test_runner::TestResult::Ignored => mutest_json::evaluation_stream::TestResult::Ignored,
                test_runner::TestResult::Failed | test_runner::TestResult::FailedMsg(_) => mutest_json::evaluation_stream::TestResult::Failed,
                test_runner::TestResult::CrashedMsg(_) => mutest_json::evaluation_stream::TestResult::Crashed,
                test_runner::TestResult::TimedOut => mutest_json::evaluation_stream::TestResult::TimedOut,
            }
        }))
    }
}

fn write_metadata<T: serde::Serialize>(write_opts: &WriteOptions, file_name: &str, data: &T) {
    let file = fs::File::create(write_opts.out_dir.join(file_name)).expect("cannot create metadata file");
    let mut buffered_file = BufWriter::new(file);

    serde_json::to_writer(&mut buffered_file, &data).expect("cannot write metadata file");
}

pub fn write_evaluation<'a, I>(
    write_opts: &WriteOptions,
    tests: &[test_runner::Test],
    unmutated_test_exec_times: &HashMap<test::TestName, Duration>,
    results: I,
    flakiness_analysis: Option<(MutationFlakinessMatrix, Duration)>,
    test_profiling_duration: Duration,
    duration: Duration,
)
where
    I: IntoIterator<Item = &'a MutationAnalysisResults>,
{
    let mut runtime_tests = mutest_json::IdxVec::with_capacity(tests.len());
    for test in tests {
        let runtime_test_id = runtime_tests.next_index();
        runtime_tests.push(mutest_json::evaluation::RuntimeTest {
            runtime_test_id,
            name: test.desc.name.as_slice().to_owned(),
            unmutated_exec_time: unmutated_test_exec_times.get(&test.desc.name).copied(),
            timeout: test.timeout,
        });
    }

    let mutation_runs = results.into_iter()
        .map(|run_results| {
            let mut overall_detections = String::with_capacity(run_results.mutation_detection_matrix.inner.len());
            for (_mutation_id, mutation_test_result) in run_results.mutation_detection_matrix.iter_detections() {
                match mutation_test_result {
                    MutationTestResult::Undetected => overall_detections.push('-'),
                    MutationTestResult::Detected => overall_detections.push('D'),
                    MutationTestResult::Crashed => overall_detections.push('C'),
                    MutationTestResult::TimedOut => overall_detections.push('T'),
                }
            }

            let mut test_detections = mutest_json::IdxVec::with_capacity(tests.len());
            for test in tests {
                let mut detections = String::with_capacity(run_results.mutation_detection_matrix.inner.len());
                for (_mutation_id, mutation_test_result) in run_results.mutation_detection_matrix.iter_test_detections(&test.desc.name) {
                    match mutation_test_result {
                        None => detections.push('.'),
                        Some(MutationTestResult::Undetected) => detections.push('-'),
                        Some(MutationTestResult::Detected) => detections.push('D'),
                        Some(MutationTestResult::Crashed) => detections.push('C'),
                        Some(MutationTestResult::TimedOut) => detections.push('T'),
                    }
                }
                // NOTE: Test detections are populated in the same order as test IDs were assigned.
                test_detections.push(detections);
            }

            let mutation_detection_matrix = mutest_json::evaluation::MutationDetectionMatrix {
                overall_detections,
                test_detections,
            };

            mutest_json::evaluation::MutationRun {
                all_mutations_detection_stats: mutest_json::evaluation::MutationDetectionStats {
                    mutation_score: match run_results.total_mutations_count {
                        0 => None,
                        _ => Some((run_results.total_mutations_count - run_results.undetected_mutations_count) as f64 / run_results.total_mutations_count as f64),
                    },
                    total_mutations_count: run_results.total_mutations_count,
                    detected_mutations_count: run_results.total_mutations_count - run_results.undetected_mutations_count,
                    timed_out_mutations_count: run_results.timed_out_mutations_count,
                    crashed_mutations_count: run_results.crashed_mutations_count,
                    undetected_mutations_count: run_results.undetected_mutations_count,
                },
                safe_mutations_detection_stats: mutest_json::evaluation::MutationDetectionStats {
                    mutation_score: match run_results.total_safe_mutations_count {
                        0 => None,
                        _ => Some((run_results.total_safe_mutations_count - run_results.undetected_safe_mutations_count) as f64 / run_results.total_safe_mutations_count as f64),
                    },
                    total_mutations_count: run_results.total_safe_mutations_count,
                    detected_mutations_count: run_results.total_safe_mutations_count - run_results.undetected_safe_mutations_count,
                    timed_out_mutations_count: run_results.timed_out_safe_mutations_count,
                    crashed_mutations_count: run_results.crashed_safe_mutations_count,
                    undetected_mutations_count: run_results.undetected_safe_mutations_count,
                },
                unsafe_mutations_detection_stats: mutest_json::evaluation::MutationDetectionStats {
                    mutation_score: match run_results.total_mutations_count - run_results.total_safe_mutations_count {
                        0 => None,
                        _ => Some(((run_results.total_mutations_count - run_results.total_safe_mutations_count) - (run_results.undetected_mutations_count - run_results.undetected_safe_mutations_count)) as f64 / (run_results.total_mutations_count - run_results.total_safe_mutations_count) as f64),
                    },
                    total_mutations_count: run_results.total_mutations_count - run_results.total_safe_mutations_count,
                    detected_mutations_count: (run_results.total_mutations_count - run_results.total_safe_mutations_count) - (run_results.undetected_mutations_count - run_results.undetected_safe_mutations_count),
                    timed_out_mutations_count: run_results.timed_out_mutations_count - run_results.timed_out_safe_mutations_count,
                    crashed_mutations_count: run_results.crashed_mutations_count - run_results.crashed_safe_mutations_count,
                    undetected_mutations_count: run_results.undetected_mutations_count - run_results.undetected_safe_mutations_count,
                },
                per_op_mutation_detection_stats: run_results.mutation_op_stats.iter()
                    .map(|(&op_name, op_stats)| {
                        let op_mutation_detection_stats = mutest_json::evaluation::MutationDetectionStats {
                            mutation_score: match op_stats.total_mutations_count {
                                0 => None,
                                _ => Some((op_stats.total_mutations_count - op_stats.undetected_mutations_count) as f64 / op_stats.total_mutations_count as f64),
                            },
                            total_mutations_count: op_stats.total_mutations_count,
                            detected_mutations_count: op_stats.total_mutations_count - op_stats.undetected_mutations_count,
                            timed_out_mutations_count: op_stats.timed_out_mutations_count,
                            crashed_mutations_count: op_stats.crashed_mutations_count,
                            undetected_mutations_count: op_stats.undetected_mutations_count,
                        };
                        (op_name.to_owned(), op_mutation_detection_stats)
                    })
                    .collect(),
                mutation_detection_matrix,
                duration: run_results.duration,
            }
        })
        .collect();

    let flakiness_analysis = flakiness_analysis.map(|(mutation_flakiness_matrix, duration)| {
        let mut overall_detection_flakiness = String::with_capacity(mutation_flakiness_matrix.inner.len());
        for (_mutation_id, detection_flakiness) in mutation_flakiness_matrix.iter_detection_flakes() {
            match detection_flakiness {
                false => overall_detection_flakiness.push('-'),
                true => overall_detection_flakiness.push('F'),
            }
        }

        let mut test_detection_flakiness = mutest_json::IdxVec::with_capacity(tests.len());
        for test in tests {
            let mut detection_flakiness = String::with_capacity(mutation_flakiness_matrix.inner.len());
            for (_mutation_id, mutation_test_flakiness) in mutation_flakiness_matrix.iter_test_flakes(&test.desc.name) {
                match mutation_test_flakiness {
                    None => detection_flakiness.push('.'),
                    Some(false) => detection_flakiness.push('-'),
                    Some(true) => detection_flakiness.push('F'),
                }
            }
            // NOTE: Test detection flakes are populated in the same order as test IDs were assigned.
            test_detection_flakiness.push(detection_flakiness);
        }

        let mutation_flakiness_matrix = mutest_json::evaluation::MutationFlakinessMatrix {
            overall_detection_flakiness,
            test_detection_flakiness,
        };

        mutest_json::evaluation::MutationFlakinessAnalysis {
            mutation_flakiness_matrix,
            duration,
        }
    });

    write_metadata(write_opts, "evaluation.json", &mutest_json::evaluation::EvaluationInfo {
        format_version: mutest_json::FORMAT_VERSION,
        mutation_runs,
        flakiness_analysis,
        tests: runtime_tests,
        test_profiling_duration,
        duration,
    });
}
