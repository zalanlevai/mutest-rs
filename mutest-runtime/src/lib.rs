#![feature(deadline_api)]
#![feature(decl_macro)]
#![feature(if_let_guard)]
#![feature(iter_array_chunks)]
#![feature(let_chains)]
#![feature(thread_id_value)]

#![feature(test)]
#![feature(internal_output_capture)]
extern crate test;

// Injected dependencies:
// Because public dependencies of mutest-runtime are injected into mutated crates,
// they are renamed to avoid name collisions.
// These crates are aliased back to their original names inside of mutest-runtime
// for convenience, so that their usual names can be used inside of this crate.
extern crate __mutest_runtime_public_dep_phf as phf;

pub mod test_runner;
pub mod thread_pool;

pub mod data_structures;

pub mod detections;
pub mod flakiness;
pub mod subsumption;

pub mod write;

mod config;
pub use config::*;

mod harness;
pub use harness::*;

mod metadata;
pub use metadata::*;

pub use test_runner::is_test_thread_active;
