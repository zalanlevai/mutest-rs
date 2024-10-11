//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![allow(unused)]

fn f() {}

macro_rules! m {
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
        format_args!("{}", i);
        format_args!("{i}");
        format_args!("{x}", x = i);
    }
}

#[test]
fn test() {
    use core::marker::PhantomData;

    // Identifiers not leaked into expansion scope:
    let v: usize = 0;
    'label: loop { break 'label; }
    m!();
    let _: usize = v;

    // Identifiers leaked into expansion scope:
    let _: Empty = Empty;
    let _: PhantomData<Never> = PhantomData;
    impl Nothing for Empty {}
    f();
    m::inner();
}

// TEST: `::serde::de::Error` trait is defined by a `macro_rules!` macro,
//        but is supposed to leak into the expansion scope.

macro_rules! declare_error_trait {
    (Trait: Sized $(+ $($supertrait:ident)::+)*) => {
        pub trait Error: Sized $(+ $($supertrait)::+)* {}
    }
}

declare_error_trait!(Trait: Sized);

struct SerdeError { a: u32 }
impl Error for SerdeError {}
