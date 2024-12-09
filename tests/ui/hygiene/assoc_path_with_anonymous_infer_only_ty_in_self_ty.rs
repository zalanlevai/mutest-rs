//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]
#![feature(async_closure)]
#![feature(coroutines)]

macro m() {
    // TEST: Anonymous function definition type.
    fn f() {}
    let _ = Box::new(f);

    // TEST: Anonymous closure type.
    let _ = Box::new(|| {});

    // TEST: Anonymous coroutine type.
    let _ = Box::new(#[coroutine] || { yield (); });

    // TEST: Anonymous async closure type.
    let _ = Box::new(async || {});
}

#[test]
fn test() {
    m!();
}
