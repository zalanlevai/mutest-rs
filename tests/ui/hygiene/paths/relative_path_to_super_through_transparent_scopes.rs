//@ build
//@ stderr: empty

#![feature(decl_macro)]

macro m() {
    // TEST: Relative path to item in parent scope through transparent inline const block.
    const _: () = {
        const fn f() {}
        const { f() };
    };
}

m!();
