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

pub mod build {
    use std::path::Path;

    const EXTERNS: &str = env!("MUTEST_RUNTIME_BUILD_EXTERNS");

    pub fn externs() -> impl Iterator<Item = (Option<&'static str>, &'static str, Option<&'static Path>)> {
        EXTERNS.split('\x1F').map(|extern_spec| {
            let (prefix, location) = match extern_spec.split_once(":") {
                Some((prefix, location)) => (Some(prefix), location),
                None => (None, extern_spec),
            };
            let (name, path) = match location.split_once("=") {
                Some((name, path)) => (name, Some(path)),
                None => (location, None),
            };

            (prefix, name, path.map(Path::new))
        })
    }
}

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
