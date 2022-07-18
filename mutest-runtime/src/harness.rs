use std::collections::HashMap;
use std::convert::Infallible;
use std::env;
use std::process::{self, Termination};
use std::time::Duration;

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
    T: Termination + Send + 'static,
    F: FnOnce() -> T + Send + 'static,
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

pub enum MutationTestResult {
    Undetected,
    Detected,
    TimedOut,
    Crashed,
}

fn run_tests(mut tests: Vec<test::TestDescAndFn>, mutations: &'static [&'static MutationMeta]) -> Result<HashMap<u32, MutationTestResult>, Infallible> {
    let mut results = HashMap::<u32, MutationTestResult>::with_capacity(mutations.len());

    for &mutation in mutations {
        results.insert(mutation.id, MutationTestResult::Undetected);
    }

    tests.retain(|test| mutations.iter().any(|m| m.reachable_from.contains_key(test.desc.name.as_slice())));

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

    test_runner::run_tests(tests, on_test_event, Some(Duration::from_secs(2)), false)?;

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

pub fn mutest_main<S>(_args: &[String], tests: Vec<test::TestDescAndFn>, mutants: &'static [&'static MutantMeta<S>], active_mutant_handle: &ActiveMutantHandle<S>) {
    let mut all_test_runs_failed_successfully = true;
    let mut total_mutations_count = 0;
    let mut undetected_mutations_count = 0;

    for &mutant in mutants {
        active_mutant_handle.replace(Some(mutant));

        println!("applying mutant with the following mutations:");
        for mutation in mutant.mutations {
            println!("- {} at {}", mutation.display_name, mutation.display_location);
        }
        println!();

        match run_tests(clone_tests(&tests), mutant.mutations) {
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

                        | MutationTestResult::Detected
                        | MutationTestResult::TimedOut
                        | MutationTestResult::Crashed => {}
                    }
                }
            }
            Err(_) => { process::exit(ERROR_EXIT_CODE); }
        }
    }

    println!("mutations: {score}. {detected} detected; {undetected} undetected; {total} total",
        score = match total_mutations_count {
            0 => "none".to_owned(),
            _ => format!("{:.2}%",(total_mutations_count - undetected_mutations_count) as f64 / total_mutations_count as f64 * 100_f64),
        },
        detected = total_mutations_count - undetected_mutations_count,
        undetected = undetected_mutations_count,
        total = total_mutations_count,
    );

    if !all_test_runs_failed_successfully {
        process::exit(ERROR_EXIT_CODE);
    }
}

pub fn mutest_main_static<S>(tests: &[&test::TestDescAndFn], mutants: &'static [&'static MutantMeta<S>], active_mutant_handle: &ActiveMutantHandle<S>) {
    let args = env::args().collect::<Vec<_>>();
    let owned_tests: Vec<_> = tests.iter().map(|test| make_owned_test(test)).collect();

    mutest_main(&args, owned_tests, mutants, active_mutant_handle)
}
