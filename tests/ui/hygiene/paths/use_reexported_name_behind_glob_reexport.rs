//@ build
//@ stderr: empty

#![feature(decl_macro)]

macro m() {
    // TEST: Path to item only accessible through a glob re-export of an "opaque" reexport
    //       that alters the visible name of the item.
    mod m {
        mod internal {
            mod impl1 {
                pub struct S;
            }

            pub use impl1::{S as ImplS};
        }

        pub use internal::*;
    }
    #[allow(unused)]
    fn use_item_through_glob_reexport_of_opaque_reexport() { let _ = m::ImplS; }
}

m!();
