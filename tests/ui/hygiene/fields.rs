//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

struct Q(usize);

struct R {
    x: isize,
}

enum E {
    A(usize),
    B { b: isize },
}

macro m($x:ident) {
    let q = Q(100);
    assert_eq!(100, q.0);
    match q {
        Q(v) => { assert_eq!(100, v); }
    }

    let r = R { x: 100 };
    assert_eq!(100, r.x);
    match r {
        R { x } => { assert_eq!(100, x); }
    }

    let ea = E::A(100);
    let eb = E::B { b: -100 };
    if let (E::A(a), E::B { b }) = (ea, eb) {
        assert_eq!(0, a as isize + b);
    }

    struct S {
        x: usize,
        $x: isize,
    }

    let s = S { x: 100, $x: -100 };
    assert_eq!(0, s.x as isize + s.$x);
    match s {
        S { x: x, $x: dx } => { assert_eq!(0, x as isize + dx); }
    }

    fn ref_ref_s(s: &&S) -> usize { s.x }
    assert_eq!(ref_ref_s(&&s), 100);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fields() {
        crate::m!(x);
    }
}
