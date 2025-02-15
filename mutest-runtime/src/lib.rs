#![feature(deadline_api)]
#![feature(decl_macro)]
#![feature(extract_if)]
#![feature(if_let_guard)]
#![feature(iter_array_chunks)]
#![feature(let_chains)]

#![feature(test)]
#![feature(internal_output_capture)]
extern crate test;

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

mod config;
pub use config::*;

mod harness;
pub use harness::*;

mod metadata;
pub use metadata::*;
