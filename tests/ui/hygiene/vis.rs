//@ build
//@ stderr: empty

#![allow(unused)]

mod inner {
    mod inner {
        pub(crate) use {};
        pub(super) use {};
        pub(self) use {};
        pub(in super::super) use {};
        pub(in crate::inner) use {};

        pub(crate) const CRATE_CONST: () = ();
        pub(super) const INNER_CONST: () = ();
        pub(self) const SELF_CONST: () = ();
        pub(in super::super) const SUPER_INNER_CONST: () = ();
        pub(in crate::inner) const IN_INNER_CONST: () = ();

        pub struct Fields {
            pub(crate) crate_field: (),
            pub(super) inner_field: (),
            pub(self) self_field: (),
            pub(in super::super) super_inner_field: (),
            pub(in crate::inner) in_inner_field: (),
        }

        extern "C" {
            pub(crate) fn crate_fn();
            pub(super) fn inner_fn();
            pub(self) fn self_fn();
            pub(in super::super) fn super_inner_fn();
            pub(in crate::inner) fn in_inner_fn();
        }

        struct Struct;
        impl Struct {
            pub(crate) fn crate_fn() {}
            pub(super) fn inner_fn() {}
            pub(self) fn self_fn() {}
            pub(in super::super) fn super_inner_fn() {}
            pub(in crate::inner) fn in_inner_fn() {}
        }

        // NOTE: AST contains variant visibilities but semantically rejected later.
        pub enum Variants {
            Variant,
        }

        // NOTE: AST contains trait / trait-impl assoc item visibilities but semantically rejected later.
        trait Trait {
            fn f();
        }
    }
}
