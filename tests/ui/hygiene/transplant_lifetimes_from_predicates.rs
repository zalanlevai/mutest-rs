//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

// NOTE: We intentionally place the trait definition outside of the macro to
//       get a different name for the `'de` lifetime in the __Visitor impl inside of the macro
//       after hygienic name mangling.
trait Visitor<'de> {
    type Value;
    fn visit() -> Result<Self::Value, ()>;
}

macro m() {
    // TEST: Transplant lifetimes from type parameter predicates.
    trait Error {}
    trait Deserializer<'de> {
        type Error: Error;
    }
    // NOTE: We intentionally use a different name for the trait's `'de` lifetime
    //       to test if we capture the right name introduced in the function definition.
    fn deserialize<'deee, __D: Deserializer<'deee>>() -> Result<(), __D::Error> { Ok(()) }

    // TEST: Transplant lifetimes from trait Self predicates.
    trait Deserialize<'de> {
        type Value;
        fn deserialize() -> Result<Self::Value, ()>;
    }

    // TEST: Transplant lifetimes from trait impl Self predicates.
    struct __Visitor;
    impl<'de> Visitor<'de> for __Visitor {
        type Value = ();
        fn visit() -> Result<Self::Value, ()> { Err(()) }
    }

    // TEST: Transplant lifetimes from type parameter predicates in impl and trait items.
    trait DeserializeWithDeserializer<'dee, __D: Deserializer<'dee>> {
        fn deserialize() -> Result<(), __D::Error>;
    }
    impl<'deee, __D: Deserializer<'deee>> DeserializeWithDeserializer<'deee, __D> for () {
        fn deserialize() -> Result<(), __D::Error> { Ok(()) }
    }
}

m!();
