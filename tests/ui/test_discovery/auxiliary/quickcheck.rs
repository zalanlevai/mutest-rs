#![crate_type = "lib"]

#[macro_export]
macro_rules! quickcheck {
    {
        $(#[$m:meta])*
        fn $fn_name:ident($($arg_name:ident : $arg_ty:ty),*) -> $ret:ty {
            $($code:tt)*
        }
    } => (
        #[test]
        $(#[$m])*
        fn $fn_name() {
            fn prop($($arg_name: $arg_ty),*) -> $ret {
                $($code)*
            }
            $crate::quickcheck(prop as fn($($arg_ty),*) -> $ret);
        }
    )
}

// Dummy implementation of the underlying quickcheck tester.
pub fn quickcheck(f: fn(u32, u32) -> bool) {
    assert!(f(0, 0));
}
