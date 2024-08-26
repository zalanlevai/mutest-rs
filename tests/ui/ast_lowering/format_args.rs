//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

#[derive(Debug)]
struct S {
    v: u32,
}

macro m() {
    fn f() {
        let n = 1;
        let msg = "msg";
        let s = S { v: 0_u32 };

        format_args!("");
        format_args!("a, b, c");
        format_args!("{n}");
        format_args!("{}, {}, {:?}", n, msg, &s);
        format_args!("{2:?}, {0}, {1}", n, msg, &s);
        format_args!("{a}, {b:?}, {c}", a = msg, b = &&s, c = n);
        format_args!("{n}, {}, {s:?}", msg);
        format_args!("{a:?}, {n}, {}, {c:?}", msg, a = s, c = s);
    }
}

m!();
