//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

use core::ops::{Deref, DerefMut};

macro m() {
    struct Wrapper { inner: u32 }

    // TEST: Paths to assoc items with implicit Self roots may point to assoc items in supertraits.
    struct S(Wrapper);
    impl Deref for S {
        type Target = Wrapper;

        fn deref(&self) -> &Self::Target {
            match self.0 { Self::Target { inner } => { let _ = inner; } };
            Self::Target { inner: self.0.inner };

            &self.0
        }
    }
    impl DerefMut for S {
        fn deref_mut(&mut self) -> &mut Self::Target {
            match self.0 { Self::Target { inner } => { let _ = inner; } };
            Self::Target { inner: self.0.inner };

            &mut self.0
        }
    }
}

m!();
