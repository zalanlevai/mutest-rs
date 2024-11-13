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

    // TEST: Transplant lifetimes from type parameter predicates in non-trait impl items.
    struct __Deserializer<__D>(std::marker::PhantomData<__D>);
    impl<'dee, __D: Deserializer<'dee>> __Deserializer<__D> {
        fn deserialize() -> Result<(), __D::Error> { Ok(()) }
    }

    // TEST: Transplant type bounds from type parameter predicates.
    struct Rgb(u8, u8, u8);
    impl std::ops::Mul<f32> for Rgb {
        type Output = Self;
        fn mul(self, _other: f32) -> Self::Output { self }
    }
    fn mul_f32<T: std::ops::Mul<f32>>() -> Result<(), T::Output> { Ok(()) }

    // TEST: Do not transplant anything if no bounds are available for the trait.
    trait DefaultWithIndirection<'de>: Default {
        type Error;
    }
    fn deserialize_default<'dee, __D: DefaultWithIndirection<'dee>>() -> Result<__D, __D::Error> { Ok(__D::default()) }

    // TEST: Handle non-trait Self references.
    trait Parser<'a> {
        fn parse() -> Self;
    }
    struct Options;
    impl<'a> Parser<'a> for Options {
        fn parse() -> Self { Self }
    }
    impl Options {
        fn new() -> Self { Self::parse() }
    }
}

m!();
