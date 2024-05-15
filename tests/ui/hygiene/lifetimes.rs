//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro lifetime_hygiene($f:ident<$a:lifetime>) {
    #[allow(unused_lifetimes, dead_code)]
    fn $f<$a, 'a>() {}
}

lifetime_hygiene!(f<'a>);
