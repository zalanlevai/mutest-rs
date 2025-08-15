//@ print-mutants
//@ run: fail
//@ stdout
//@ stderr: empty
//@ mutation-operators: bool_expr_negate
//@ run-flags: -v --exhaustive

//! Mutations may lead to infinite, or long-running test cases in certain cases.
//! For threaded evaluation of such mutations, timeouts are necessary.
//! Because threads cannot be safely cancelled externally in any environment
//! (e.g. POSIX threads, or Windows threads),
//! we instead inject "cancellation points" into the generated program.
//!
//! This test specficically tests the most important cancellation point for correctness:
//! triggered by a mutated test thread re-entering any active code subsitution point
//! within the program.
//! This is important, because without this forced cancellation, such a lingering test thread
//! would continue execution of other mutated code, leading to undefined behavior
//! composed of the partial behaviors of two or more mutations, changing over time.

use std::sync::atomic::{self, AtomicBool};

static FIRST_MUTATION_NOT_CANCELLED_AFTER_REENTRY: AtomicBool = AtomicBool::new(false);

fn needs_cancellation() {
    #[mutest::ignore]
    FIRST_MUTATION_NOT_CANCELLED_AFTER_REENTRY.store(false, atomic::Ordering::SeqCst);

    loop {
        // NOTE: With a mutation swapping `true` to `false`, this first mutation will cause
        //       an infinite loop, leading to a timeout.
        if true {
            // NOTE: Without cancellation, the moment the second mutation is enabled,
            //       this first, lingering mutation test thread would have its behavior changed back
            //       to that of the original code, meaning that execution would enter this block.
            //       To signify this occurance using something we can assert, we use a global variable
            //       to signal to the assertion on the test thread of the second, synthetic mutation.
            #[mutest::ignore]
            FIRST_MUTATION_NOT_CANCELLED_AFTER_REENTRY.store(true, atomic::Ordering::SeqCst);

            break;
        }
    }
}

#[test]
fn test1() {
    needs_cancellation();
}

fn trigger_second_mutation() {
    // NOTE: This `if false` both triggers a second mutation (which we use to assert the cancellation),
    //       and ensures that the assertion does not happen during the profiling run.
    if false {
        assert_first_mutation_cancelled()
    }
}

#[mutest::skip]
fn assert_first_mutation_cancelled() {
    // NOTE: This assertion succeeds iff the first mutation was cancelled,
    //       which corresponds to a lack of detection of the second mutation
    //       (i.e. the code of the second mutation ran without any interruption from the first mutation).
    assert_eq!(false, FIRST_MUTATION_NOT_CANCELLED_AFTER_REENTRY.load(atomic::Ordering::SeqCst));
}

#[test]
fn test2() {
    trigger_second_mutation();
}
