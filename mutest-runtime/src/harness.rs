use std::env;
use std::process;

use crate::metadata::MutantMeta;

mod test {
    pub use ::test::*;
    pub use ::test::test::*;
}

// TODO: Consider using `MaybeUninit<Mutant>` for the cell type if the harness never runs the
//       program without any mutations applied.
pub struct ActiveMutantHandle<'a, S>(parking_lot::RwLock<Option<&'a MutantMeta<'a, S>>>);

impl <'a, S> ActiveMutantHandle<'a, S> {
    pub const fn empty() -> Self {
        Self(parking_lot::const_rwlock(None))
    }

    pub const fn with(v: &'a MutantMeta<'a, S>) -> Self {
        Self(parking_lot::const_rwlock(Some(v)))
    }

    pub fn borrow(&self) -> Option<&'a MutantMeta<'a, S>> {
        *self.0.read()
    }

    pub fn replace(&self, v: Option<&'a MutantMeta<'a, S>>) {
        *self.0.write() = v;
    }
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

pub fn mutest_main<'m, S>(args: &[String], tests: Vec<test::TestDescAndFn>, mutants: &'m [MutantMeta<'m, S>], active_mutant_handle: &ActiveMutantHandle<'m, S>) {
    let opts = match test::parse_opts(args) {
        Some(Ok(o)) => o,
        Some(Err(msg)) => {
            eprintln!("error: {}", msg);
            process::exit(ERROR_EXIT_CODE);
        }
        None => return,
    };

    let mut all_test_runs_failed_successfully = true;

    for mutant in mutants {
        active_mutant_handle.replace(Some(mutant));

        println!("applying mutant with the following mutations:");
        for mutation in mutant.mutations {
            println!("- {} at {}", mutation.display_name, mutation.display_location);
        }

        match test::run_tests_console(&opts, clone_tests(&tests)) {
            Ok(true) => {
                all_test_runs_failed_successfully = false;
                print!("{}", mutant.undetected_diagnostic);
            }
            Ok(false) => {}
            Err(err) => {
                eprintln!("error: io error: {:?}", err);
                process::exit(ERROR_EXIT_CODE);
            }
        }
    }

    if !all_test_runs_failed_successfully {
        process::exit(ERROR_EXIT_CODE);
    }
}

pub fn mutest_main_static<'m, S>(tests: &[&test::TestDescAndFn], mutants: &'m [MutantMeta<'m, S>], active_mutant_handle: &ActiveMutantHandle<'m, S>) {
    let args = env::args().collect::<Vec<_>>();
    let owned_tests: Vec<_> = tests.iter().map(|test| make_owned_test(test)).collect();

    mutest_main(&args, owned_tests, mutants, active_mutant_handle)
}