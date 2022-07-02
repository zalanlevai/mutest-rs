#![feature(deadline_api)]
#![feature(decl_macro)]
#![feature(let_chains)]
#![feature(let_else)]

#![feature(test)]
#![feature(internal_output_capture)]

// FIXME: Switch to using `paring_lot` from crates.io once proper dependency resolution in generated
//        code is implemented. Currently we conflict with the version used internally by rustc.
#![feature(rustc_private)]
extern crate parking_lot;

extern crate test;

pub mod test_runner;

mod harness;
pub use harness::*;

mod metadata;
pub use metadata::*;
