#![feature(deadline_api)]
#![feature(decl_macro)]
#![feature(drain_filter)]
#![feature(let_chains)]
#![feature(let_else)]

#![feature(test)]
#![feature(internal_output_capture)]

// FIXME: Switch to using `paring_lot` from crates.io once proper dependency resolution in generated
//        code is implemented. Currently we conflict with the version used internally by rustc.
#![feature(rustc_private)]
extern crate parking_lot;

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

mod config;
pub use config::*;

mod harness;
pub use harness::*;

mod metadata;
pub use metadata::*;
