#![feature(test)]

// FIXME: Switch to using `paring_lot` from crates.io once proper dependency resolution in generated
//        code is implemented. Currently we conflict with the version used internally by rustc.
#![feature(rustc_private)]
extern crate parking_lot;

extern crate test;

mod harness;
pub use harness::*;

mod metadata;
pub use metadata::*;
