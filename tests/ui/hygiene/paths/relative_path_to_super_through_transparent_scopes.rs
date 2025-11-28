//@ build
//@ stderr: empty

#![feature(decl_macro)]

macro m() {
    // TEST: Relative path to item in parent scope through transparent inline const block.
    const _: () = {
        const fn f() {}
        const { f() };
    };

    // TEST: Relative path to item in parent scope through transparent closure.
    const _: () = {
        const LEN: usize = 128;
        let _ = || {
            let _buf = [0; LEN];
        };
    };
}

m!();
