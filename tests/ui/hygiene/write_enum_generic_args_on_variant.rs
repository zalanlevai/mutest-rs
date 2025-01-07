//@ build
//@ stderr: empty

//! This test ensures that we write out enum generics on their variants
//! for paths pointing to enum variants.
//! This is needed in cases where the path to an enum variant does not mention the enum itself
//! (and the generic args of the enum type cannot be inferred otherwise).
//! Such paths may be generated during sanitization in the presence of more direct visible re-exports.
//! In such cases, the existing generic args on the enum must be moved to the enum variant to
//! retain the same semantics.

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    mod inner {
        pub enum Enum<T> {
            Variant(std::marker::PhantomData<T>),
        }

        pub use Enum::Variant;
    }

    #[test]
    fn test() {
        // TEST: Paths to enum variants, generated during sanitization, may not mention the enum itself, but
        //       we must still retain the enum generic args, which can appear on the variant itself.
        //       Here, this happens because a more direct visible re-export of the enum variant is available.
        let _ = inner::Enum::<()>::Variant(std::marker::PhantomData);

        // TEST: Leave variant generics as-is, ensuring that the generic args remain.
        let _ = inner::Variant::<()>(std::marker::PhantomData);

        // TEST: Leave paths to enums as-is, ensuring that the generic args remain.
        let _: Option<inner::Enum<()>> = None;

        // TEST: `Option` variants are directly available in the prelude and we resolve to them directly,
        //       losing the generic enum path segment.
        let _ = Option::<()>::None;

        // TEST: This may be required for inference of other expressions to succeed.
        let mut chain = std::iter::empty();
        assert_eq!(chain.next(), Option::<()>::None);
    }
}

m!();
