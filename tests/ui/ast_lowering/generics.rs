//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait I {}
    trait Dummy {}

    trait GenericTraitWithMixedPositionTypePredicates<U: I, V>
    where
        V: Dummy,
        U: Dummy,
    {}
}

#[test]
fn test() {
    m!();
}
