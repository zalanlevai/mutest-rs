//@ build
//@ stderr: empty

#![feature(decl_macro)]

#[cfg(test)]
mod tests {
    mod somewhere {
        #[allow(unused)]
        pub(super) struct A { pub v: usize }

        pub(super) mod inner {
            pub(in super::super) struct A;
        }

        pub(crate) macro capturing_macro($f1:ident, $f2:ident, $f3:ident, $m1:ident, $f4:ident) {
            fn $f1() -> A { A { v: 0 } }
            fn $f2() -> inner::A { inner::A }
            fn $f3() -> super::somewhere::inner::A { self::inner::A }

            mod $m1 {
                pub fn $f4() -> super::inner::A { super::inner::A }
            }
        }
    }

    #[test]
    fn test_captured_items() {
        somewhere::capturing_macro!(f1, f2, f3, m1, f4);
        let _: somewhere::A = f1();
        let _: somewhere::inner::A = f2();
        let _: somewhere::inner::A = f3();
        let _: somewhere::inner::A = m1::f4();
    }
}
