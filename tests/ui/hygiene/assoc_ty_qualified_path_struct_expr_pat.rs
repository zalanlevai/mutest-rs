//@ build
//@ stderr: empty

#![feature(decl_macro)]
#![feature(more_qualified_paths)]

#![allow(unused)]

use core::ops::{Deref, DerefMut, Index};

macro m() {
    struct Wrapper { inner: u32 }

    // TEST: Simple unambiguous resolution with no generic arguments.
    struct DerefOnly(Wrapper);
    impl Deref for DerefOnly {
        type Target = Wrapper;
        fn deref(&self) -> &Self::Target { &self.0 }
    }
    impl DerefOnly {
        fn test(&self) -> <Self as Deref>::Target {
            match self.0 { <Self as Deref>::Target { inner } => { let _ = inner; } };
            <Self as Deref>::Target { inner: self.0.inner }
        }
    }

    // TEST: Ambiguous impls differentiated by generic arguments.
    struct IndexImpl([Wrapper; 3]);
    impl Index<usize> for IndexImpl {
        type Output = Wrapper;
        fn index(&self, index: usize) -> &Self::Output { &self.0[index] }
    }
    impl Index<u8> for IndexImpl {
        type Output = Wrapper;
        fn index(&self, index: u8) -> &Self::Output { &self.0[index as usize] }
    }
    impl IndexImpl {
        fn first(&self) -> <Self as Index<usize>>::Output {
            match self.0[0] { <Self as Index<usize>>::Output { inner } => { let _ = inner; } };
            <Self as Index<usize>>::Output { inner: self.0[0].inner }
        }
    }
}

m!();
