//@ fail
//@ stderr

#![allow(unused)]

mod def {
    use inner::S;

    mod inner {
        pub struct S(u32);

        fn f() {
            let _ = crate::def::S(0);
        }
    }
}
