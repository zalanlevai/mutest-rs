//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    // TEST: Transplant lifetimes from type parameter predicates.

    trait Error {}

    trait Deserializer<'de> {
        type Error: Error;
    }

    fn deserialize<'de, __D: Deserializer<'de>>() -> Result<(), __D::Error> { Ok(()) }

    // TEST: Transplant lifetimes from Self predicates.

    trait Visitor<'de> {
        type Value;
        fn visit() -> Result<Self::Value, ()>;
    }

    struct __Visitor;
    impl<'de> Visitor<'de> for __Visitor {
        type Value = ();
        fn visit() -> Result<Self::Value, ()> { Err(()) }
    }
}

m!();
