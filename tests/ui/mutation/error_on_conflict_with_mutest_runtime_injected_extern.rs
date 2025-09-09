//@ build: fail
//@ stderr
//@ aux-build: phf.rs
//@ rustc-flags: --extern __mutest_runtime_public_dep_phf=target/mutest_test/debug/deps/auxiliary/libphf.rlib

// NOTE: A mutable function is required to generate conflicting phf-dependent harness statements.
fn mutable_add_fn(a: usize, b: usize) -> usize {
    a + b
}

#[test]
fn test() {
    __mutest_runtime_public_dep_phf::incompatible_phf_map!();

    // NOTE: Mutations are required to generate conflicting phf-dependent harness statements.
    assert_eq!(3, mutable_add_fn(1, 2));
}
