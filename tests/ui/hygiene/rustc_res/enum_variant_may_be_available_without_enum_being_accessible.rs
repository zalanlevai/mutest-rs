//@ build
//@ stderr: empty
//@ mutest-flags: --Zno-sanitize-macro-expns

mod def {
    mod inner {
        // NOTE: Enum has to be public for public re-exports of its variants to be allowed.
        //       The variants effectively take on the same local visibility as the parent enum.
        pub enum Enum<T> {
            Variant(std::marker::PhantomData<T>),
        }

        pub use Enum::Variant;
    }

    // NOTE: We only publicly re-export the enum variant,
    //       making the enum itself inaccessible from outside of this module.
    pub use inner::Variant;

    #[test]
    fn test() {
        // NOTE: We can always give enum generic args to its variant constructors,
        //       even when referring to the enum directly.
        let _ = inner::Enum::Variant::<()>(std::marker::PhantomData);
    }
}

#[test]
fn test() {
    // NOTE: We can still give the required enum generic args to its variant constructor.
    let _ = def::Variant::<()>(std::marker::PhantomData);
}
