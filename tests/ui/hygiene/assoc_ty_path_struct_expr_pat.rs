//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

use core::ops::{Deref, DerefMut};

macro m() {
    struct Wrapper { inner: u32 }

    // TEST: `Self::Assoc` path.
    struct DerefOnly(Wrapper);
    impl Deref for DerefOnly {
        type Target = Wrapper;

        fn deref(&self) -> &Self::Target {
            match self.0 { Self::Target { inner } => { let _ = inner; } };
            Self::Target { inner: self.0.inner };

            &self.0
        }
    }

    // TEST: Avoid attempting to resolve unresolvable type-relative assoc paths.
    fn foo<T: DerefMut>() -> Option<T::Target>
    where
        T::Target: Sized,
    { None }
}

m!();
