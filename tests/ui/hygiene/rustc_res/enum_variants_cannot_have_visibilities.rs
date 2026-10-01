//@ rustc
//@ build: fail
//@ stderr

mod inner {
    pub enum Variants {
        pub PubVariant,
        pub(crate) CrateVariant,
        pub(super) InnerVariant,
        pub(self) SelfVariant,
        pub(in crate::inner) InInnerVariant,
    }
}
