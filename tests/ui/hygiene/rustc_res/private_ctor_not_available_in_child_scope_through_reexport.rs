//@ fail
//@ stderr

#![allow(unused)]

use def::S;

pub mod def {
    pub struct S(u32);

    mod inner {
        fn f() {
            let _ = crate::S(0);
        }
    }
}
