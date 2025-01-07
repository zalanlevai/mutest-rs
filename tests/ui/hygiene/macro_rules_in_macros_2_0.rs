//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

fn f() {}

macro outer_m() {
    macro_rules! inner_m {
        () => {
            // Identifiers to leak into expansion scope:
            pub struct Empty;
            pub enum Never {}
            pub trait Nothing {}
            pub fn f() {}
            pub mod m {
                pub fn inner() {}
            }

            // Identifiers to not leak into expansion scope:
            let v: Empty = Empty;
            'label: loop { break 'label; }
            $crate::f;

            // Ensure that locals are identified correctly.
            let mut i: i32 = 1;
            i = 2;
        };
    }

    // Identifiers not leaked into expansion scope:
    let v: usize = 0;
    'label: loop { break 'label; }
    inner_m!();
    let _: usize = v;

    // TODO: Assert identifiers leaked into expansion scope.
    // NOTE: This is messy because macro_rules and macros 2.0 interactions are currently unsupported, and
    //       resolution across these boundaries is not well-defined.
}

#[test]
fn test() {
    outer_m!();
}
