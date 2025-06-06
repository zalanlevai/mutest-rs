//@ build
//@ stderr: empty
//@ aux-build: serde.rs
//@ aux-build: phf.rs
//@ rustc-flags: --extern serde=target/mutest_test/debug/deps/auxiliary/libserde.rlib
//@ rustc-flags: --extern phf=target/mutest_test/debug/deps/auxiliary/libphf.rlib

// NOTE: A mutable function is required to generate conflicting phf-dependent harness statements.
fn mutable_add_fn(a: usize, b: usize) -> usize {
    a + b
}

#[test]
fn test() {
    serde::incompatible_serialize();
    phf::incompatible_phf_map!();

    // NOTE: Mutations are required to generate conflicting phf-dependent harness statements.
    assert_eq!(3, mutable_add_fn(1, 2));
}
