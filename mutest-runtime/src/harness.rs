use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::Infallible;
use std::env;
use std::process::{self, Termination};
use std::time::{Duration, Instant};

use crate::config::{self, Options};
use crate::metadata::{MutantMeta, MutationMeta};
use crate::test_runner;

mod test {
    pub use ::test::*;
    pub use ::test::test::*;
}

// TODO: Consider using `MaybeUninit<Mutant>` for the cell type if the harness never runs the
//       program without any mutations applied.
pub struct ActiveMutantHandle<S: 'static>(parking_lot::RwLock<Option<&'static MutantMeta<S>>>);

impl <S> ActiveMutantHandle<S> {
    pub const fn empty() -> Self {
        Self(parking_lot::const_rwlock(None))
    }

    pub const fn with(v: &'static MutantMeta<S>) -> Self {
        Self(parking_lot::const_rwlock(Some(v)))
    }

    pub fn borrow(&self) -> Option<&'static MutantMeta<S>> {
        *self.0.read()
    }

    pub fn replace(&self, v: Option<&'static MutantMeta<S>>) {
        *self.0.write() = v;
    }
}

// NOTE: `mutest_runtime::wrap` is currently a no-op test wrapper. The codegen for it was left intact if we ever need
//       such functionality in the future.
pub fn wrap<T, F>(test: F) -> T
where
    T: Termination + 'static,
    F: FnOnce() -> T + 'static,
{
    test()
}

const ERROR_EXIT_CODE: i32 = 101;

fn make_owned_test(test: &test::TestDescAndFn) -> test::TestDescAndFn {
    match test.testfn {
        test::TestFn::StaticTestFn(f) => test::TestDescAndFn { testfn: test::TestFn::StaticTestFn(f), desc: test.desc.clone() },
        test::TestFn::StaticBenchFn(f) => test::TestDescAndFn { testfn: test::TestFn::StaticBenchFn(f), desc: test.desc.clone() },
        _ => panic!("non-static tests passed to mutest_runtime::mutest_main"),
    }
}

fn clone_tests(tests: &Vec<test::TestDescAndFn>) -> Vec<test::TestDescAndFn> {
    tests.iter().map(make_owned_test).collect()
}

struct ProfiledTest {
    pub test: test::TestDescAndFn,
    pub result: test_runner::TestResult,
    pub exec_time: Option<Duration>,
}

fn profile_tests(tests: Vec<test::TestDescAndFn>) -> Result<Vec<ProfiledTest>, Infallible> {
    let mut remaining_tests = clone_tests(&tests);
    let mut profiled_tests = Vec::<ProfiledTest>::with_capacity(tests.len());

    let on_test_event = |event, _remaining_tests: &mut Vec<(test::TestId, test::TestDescAndFn)>| -> Result<_, Infallible> {
        match event {
            test_runner::TestEvent::Result(test) => {
                let test_desc_and_fn = remaining_tests
                    .drain_filter(|t| t.desc.name == test.desc.name)
                    .next().expect("completed test not found amongst remaining tests");

                profiled_tests.push(ProfiledTest {
                    test: test_desc_and_fn,
                    result: test.result,
                    exec_time: test.exec_time,
                });
            }
            _ => {}
        }

        Ok(test_runner::Flow::Continue)
    };

    test_runner::run_tests(tests, on_test_event, None, false)?;

    Ok(profiled_tests)
}

fn sort_profiled_tests_by_exec_time(profiled_tests: &mut Vec<ProfiledTest>) {
    profiled_tests.sort_by(|a, b| {
        match (a.exec_time, b.exec_time) {
            (Some(exec_time_a), Some(exec_time_b)) => Ord::cmp(&exec_time_a, &exec_time_b),
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            (None, None) => Ordering::Equal,
        }
    });
}

fn prioritize_tests_by_distance(tests: &mut Vec<test::TestDescAndFn>, mutations: &'static [&'static MutationMeta]) {
    tests.sort_by(|a, b| {
        let distance_a = mutations.iter().filter_map(|&m| m.reachable_from.get(a.desc.name.as_slice())).reduce(Ord::min);
        let distance_b = mutations.iter().filter_map(|&m| m.reachable_from.get(b.desc.name.as_slice())).reduce(Ord::min);

        match (distance_a, distance_b) {
            (Some(distance_a), Some(distance_b)) => Ord::cmp(distance_a, distance_b),
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            (None, None) => Ordering::Equal,
        }
    });
}

fn maximize_mutation_parallelism(tests: &mut Vec<test::TestDescAndFn>, mutations: &'static [&'static MutationMeta]) {
    let mut parallelized_tests = Vec::<test::TestDescAndFn>::with_capacity(tests.len());

    while !tests.is_empty() {
        for mutation in mutations {
            if let Some(test) = tests.iter()
                .position(|t| mutation.reachable_from.contains_key(t.desc.name.as_slice()))
                .map(|i| tests.remove(i))
            {
                parallelized_tests.push(test);
            }
        }
    }

    *tests = parallelized_tests;
}

pub enum MutationTestResult {
    Undetected,
    Detected,
    TimedOut,
    Crashed,
}

fn run_tests(mut tests: Vec<test::TestDescAndFn>, mutations: &'static [&'static MutationMeta], test_timeout: Option<Duration>) -> Result<HashMap<u32, MutationTestResult>, Infallible> {
    let mut results = HashMap::<u32, MutationTestResult>::with_capacity(mutations.len());

    for &mutation in mutations {
        results.insert(mutation.id, MutationTestResult::Undetected);
    }

    tests.retain(|test| mutations.iter().any(|m| m.reachable_from.contains_key(test.desc.name.as_slice())));
    maximize_mutation_parallelism(&mut tests, mutations);

    let total_tests_count = tests.len();
    let mut completed_tests_count = 0;

    let on_test_event = |event, remaining_tests: &mut Vec<(test::TestId, test::TestDescAndFn)>| -> Result<_, Infallible> {
        match event {
            test_runner::TestEvent::Result(test) => {
                completed_tests_count += 1;

                let mutation = mutations.iter().find(|m| m.reachable_from.contains_key(test.desc.name.as_slice()))
                    .expect("only tests which reach mutations should have been run: no mutation is reachable from this test");

                match test.result {
                    | test_runner::TestResult::Ignored
                    | test_runner::TestResult::Ok => {
                        return Ok(test_runner::Flow::Continue);
                    }

                    | test_runner::TestResult::Failed
                    | test_runner::TestResult::FailedMsg(_) => {
                        results.insert(mutation.id, MutationTestResult::Detected);
                    }

                    test_runner::TestResult::TimedOut => {
                        results.insert(mutation.id, MutationTestResult::TimedOut);
                    }
                }

                remaining_tests.retain(|(_, test)| !mutation.reachable_from.contains_key(test.desc.name.as_slice()));

                if results.iter().all(|(_, result)| !matches!(result, MutationTestResult::Undetected)) {
                    return Ok(test_runner::Flow::Stop);
                }
            }
            _ => {}
        }

        Ok(test_runner::Flow::Continue)
    };

    test_runner::run_tests(tests, on_test_event, test_timeout, false)?;

    println!("ran {completed} out of {total} {descr}",
        completed = completed_tests_count,
        total = total_tests_count,
        descr = match total_tests_count {
            1 => "test",
            _ => "tests",
        },
    );
    println!();

    Ok(results)
}

pub fn mutest_main<S>(args: &[&str], tests: Vec<test::TestDescAndFn>, mutants: &'static [&'static MutantMeta<S>], active_mutant_handle: &ActiveMutantHandle<S>) {
    let opts = Options {
        test_timeout: config::TestTimeout::Auto,
        test_ordering: config::TestOrdering::ExecTime,
        report_timings: args.contains(&"--timings"),
    };

    let t_start = Instant::now();

    println!("profiling reference test run");
    let t_test_profiling_start = Instant::now();
    let mut profiled_tests = match profile_tests(tests) {
        Ok(tests) => tests,
        Err(_) => { process::exit(ERROR_EXIT_CODE); }
    };
    let test_profiling_duration = t_test_profiling_start.elapsed();

    if profiled_tests.iter().any(|test| !matches!(test.result, test_runner::TestResult::Ignored | test_runner::TestResult::Ok)) {
        println!("not all tests passed");
        process::exit(ERROR_EXIT_CODE);
    }

    sort_profiled_tests_by_exec_time(&mut profiled_tests);

    for profiled_test in &profiled_tests {
        match profiled_test.exec_time {
            Some(exec_time) => println!("{} took {:?}", profiled_test.test.desc.name.as_slice(), exec_time),
            None => println!("{} was not profiled", profiled_test.test.desc.name.as_slice()),
        }
    }
    println!();

    let auto_test_timeout = profiled_tests.last().and_then(|profiled_test| profiled_test.exec_time)
        .map(|d| d + Ord::max(d.mul_f32(0.1), Duration::from_secs(1)));

    let tests = profiled_tests.into_iter().map(|profiled_test| profiled_test.test).collect();

    let test_timeout = match opts.test_timeout {
        config::TestTimeout::None => None,
        config::TestTimeout::Auto => Some(auto_test_timeout.expect("no test timeout could be deduced automatically")),
        config::TestTimeout::Explicit(test_timeout) => {
            if let Some(auto_test_timeout) = auto_test_timeout {
                if test_timeout < auto_test_timeout {
                    println!("warning: explicit test timeout is less than the recommended test timeout based on the profiled reference run\n");
                }
            }

            Some(test_timeout)
        }
    };

    let mut all_test_runs_failed_successfully = true;
    let mut total_mutations_count = 0;
    let mut undetected_mutations_count = 0;
    let mut timed_out_mutations_count = 0;
    let mut crashed_mutations_count = 0;

    let t_mutation_testing_start = Instant::now();
    for &mutant in mutants {
        active_mutant_handle.replace(Some(mutant));

        println!("applying mutant with the following mutations:");
        for mutation in mutant.mutations {
            println!("- {} at {}", mutation.display_name, mutation.display_location);
        }
        println!();

        let mut tests = clone_tests(&tests);

        if let config::TestOrdering::MutationDistance = opts.test_ordering {
            prioritize_tests_by_distance(&mut tests, mutant.mutations);
        }

        match run_tests(tests, mutant.mutations, test_timeout) {
            Ok(results) => {
                total_mutations_count += mutant.mutations.len();

                for &mutation in mutant.mutations {
                    let Some(result) = results.get(&mutation.id) else { unreachable!() };

                    match result {
                        MutationTestResult::Undetected => {
                            all_test_runs_failed_successfully = false;

                            undetected_mutations_count += 1;
                            print!("{}", mutation.undetected_diagnostic);
                        }

                        MutationTestResult::Detected => {}
                        MutationTestResult::TimedOut => {
                            timed_out_mutations_count += 1;
                        }
                        MutationTestResult::Crashed => {
                            crashed_mutations_count += 1;
                        }
                    }
                }
            }
            Err(_) => { process::exit(ERROR_EXIT_CODE); }
        }
    }
    let mutation_testing_duration = t_mutation_testing_start.elapsed();

    println!("mutations: {score}. {detected} detected ({timed_out} timed out; {crashed} crashed); {undetected} undetected; {total} total",
        score = match total_mutations_count {
            0 => "none".to_owned(),
            _ => format!("{:.2}%",(total_mutations_count - undetected_mutations_count) as f64 / total_mutations_count as f64 * 100_f64),
        },
        detected = total_mutations_count - undetected_mutations_count,
        timed_out = timed_out_mutations_count,
        crashed = crashed_mutations_count,
        undetected = undetected_mutations_count,
        total = total_mutations_count,
    );

    if opts.report_timings {
        println!("\nfinished in {total:.2?} (profiling {profiling:.2?}; tests {tests:.2?})",
            total = t_start.elapsed(),
            profiling = test_profiling_duration,
            tests = mutation_testing_duration,
        );
    }

    if !all_test_runs_failed_successfully {
        process::exit(ERROR_EXIT_CODE);
    }
}

pub fn mutest_main_static<S>(tests: &[&test::TestDescAndFn], mutants: &'static [&'static MutantMeta<S>], active_mutant_handle: &ActiveMutantHandle<S>) {
    let args = env::args().collect::<Vec<_>>();
    let args = args.iter().map(String::as_ref).collect::<Vec<_>>();
    let owned_tests: Vec<_> = tests.iter().map(|test| make_owned_test(test)).collect();

    mutest_main(&args, owned_tests, mutants, active_mutant_handle)
}
