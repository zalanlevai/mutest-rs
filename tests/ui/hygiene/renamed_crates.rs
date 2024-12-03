//@ build
//@ stderr: empty
//@ aux-build: dummy_crate.rs
//@ rustc-flags: --extern dummy_crate_renamed=target/mutest_test/debug/deps/auxiliary/libdummy_crate.rlib
//@ mutest-flags: --Zsanitize-macro-expns

use dummy_crate_renamed::{S, bar};

#[test]
fn test() {
    dummy_crate_renamed::foo();
    bar();

    let _ = S::new();
}
