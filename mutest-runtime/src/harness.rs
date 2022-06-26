use std::env;
use std::process::{self, Termination};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use crate::metadata::MutantMeta;

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

fn execute_with_timeout<T, F>(test: F, timeout: Duration) -> T
where
    T: Termination + Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    let (tx, rx) = mpsc::channel();

    let handle = thread::Builder::new()
        .name(format!("{} <wrapped>", thread::current().name().unwrap()))
        .spawn(move || {
            match tx.send(test()) {
                Ok(()) => {}
                Err(_) => {}
            }
        })
        .unwrap();

    let termination = rx.recv_timeout(timeout);

    if handle.is_finished() {
        match handle.join() {
            Ok(_) => {}
            Err(_) => panic!("wrapped thread panicked"),
        }
    }

    match termination {
        Ok(t) => t,
        Err(_) => panic!("test timed out after {timeout:?}"),
    }
}

pub fn wrap<T, F>(test: F) -> T
where
    T: Termination + Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    execute_with_timeout(test, Duration::from_millis(2000))
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

pub fn mutest_main<S>(args: &[String], tests: Vec<test::TestDescAndFn>, mutants: &'static [&'static MutantMeta<S>], active_mutant_handle: &ActiveMutantHandle<S>) {
    let opts = match test::parse_opts(args) {
        Some(Ok(o)) => o,
        Some(Err(msg)) => {
            eprintln!("error: {}", msg);
            process::exit(ERROR_EXIT_CODE);
        }
        None => return,
    };

    let mut all_test_runs_failed_successfully = true;
    let mut total_mutants_count = 0;
    let mut undetected_mutants_count = 0;
    let mut total_mutations_count = 0;
    let mut undetected_mutations_count = 0;

    for &mutant in mutants {
        active_mutant_handle.replace(Some(mutant));

        println!("applying mutant with the following mutations:");
        for mutation in mutant.mutations {
            println!("- {} at {}", mutation.display_name, mutation.display_location);
        }

        match test::run_tests_console(&opts, clone_tests(&tests)) {
            Ok(all_tests_passed) => {
                total_mutants_count += 1;
                total_mutations_count += mutant.mutations.len();

                if all_tests_passed {
                    all_test_runs_failed_successfully = false;
                    undetected_mutants_count += 1;
                    undetected_mutations_count += mutant.mutations.len();

                    for mutation in mutant.mutations {
                        print!("{}", mutation.undetected_diagnostic);
                    }
                }
            }
            Err(err) => {
                eprintln!("error: io error: {:?}", err);
                process::exit(ERROR_EXIT_CODE);
            }
        }
    }

    println!("mutants:   {score}. {detected} detected; {undetected} undetected; {total} total",
        score = match total_mutants_count {
            0 => "none".to_owned(),
            _ => format!("{:.2}%", (total_mutants_count - undetected_mutants_count) as f64 / total_mutants_count as f64 * 100_f64),
        },
        detected = total_mutants_count - undetected_mutants_count,
        undetected = undetected_mutants_count,
        total = total_mutants_count,
    );
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
