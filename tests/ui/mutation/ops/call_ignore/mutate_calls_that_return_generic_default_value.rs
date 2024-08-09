//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete, call_value_default_shadow

#![allow(unused)]

fn f() {
    fn custom_default<T: Default>(_: ()) -> T {
        fn custom_default_impl<T: Default>(_: ()) -> T {
            T::default()
        }

        custom_default_impl::<T>(())
    }

    let default_usize = custom_default::<usize>(());
}

#[test]
fn test() {
    f();
}
