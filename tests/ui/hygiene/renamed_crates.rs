//@ build
//@ stderr: empty
//@ aux-build: dummy_crate.rs
//@ rustc-flags: --extern dummy_crate_renamed=target/mutest_test/debug/deps/auxiliary/libdummy_crate.rlib
//@ rustc-flags: --extern dummy_crate_reexport_unused=target/mutest_test/debug/deps/auxiliary/libdummy_crate_reexport.rlib

use dummy_crate_renamed::{S, bar};

#[test]
fn test() {
    dummy_crate_renamed::foo();
    bar();

    let _ = S::new();
}
