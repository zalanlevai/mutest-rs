//@ build: fail
//@ stderr
//@ mutest-flags: -Z no-sanitize-macro-expns

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
