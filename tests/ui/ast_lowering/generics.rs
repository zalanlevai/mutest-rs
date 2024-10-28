//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    trait I {}
    trait Dummy {}

    trait GenericTraitWithMixedPositionTypePredicates<U: I, V>
    where
        V: Dummy,
        U: Dummy,
    {}
}
