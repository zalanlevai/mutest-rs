#![feature(decl_macro)]

#![no_std]

// Injected dependencies:
// Because public dependencies of mutest-runtime are injected into mutated crates,
// they are renamed to avoid name collisions.
// These crates are aliased back to their original names inside of mutest-runtime
// for convenience, so that their usual names can be used inside of this crate.
extern crate __mutest_runtime_public_dep_phf as phf;

mod harness;
pub use harness::*;

mod metadata;
pub use metadata::*;

// NOTE: This symbol is assumed to exist by codegen in mutest-emit, however
//       thread-based test cancellations are unused in the embedded runtime.
pub fn is_test_thread_active() -> bool { true }
