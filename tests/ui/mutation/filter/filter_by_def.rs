//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v --filter-mutations=def:inner::mutable_fn

#![allow(unused)]

const fn immutable_fn() {}

fn unreached_mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

mod inner {
    fn mutable_fn(a: u32, b: u32) -> u32 {
        a + b
    }

    fn other_mutable_fn(a: u32, b: u32) -> u32 {
        a + b
    }

    #[test]
    fn test() {
        assert_eq!(5, mutable_fn(2, 3));
        assert_eq!(5, other_mutable_fn(2, 3));
    }
}
