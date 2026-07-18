//@ build: fail
//@ stderr
//@ mutest-flags: -Z no-sanitize-macro-expns

mod inner {
    pub enum Variants {
        pub PubVariant,
        pub(crate) CrateVariant,
        pub(super) InnerVariant,
        pub(self) SelfVariant,
        pub(in crate::inner) InInnerVariant,
    }
}
