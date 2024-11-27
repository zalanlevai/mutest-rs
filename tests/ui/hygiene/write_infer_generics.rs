//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(allocator_api)]
#![feature(decl_macro)]

#![allow(unused)]

use std::alloc::{Allocator, Global};
use std::marker::PhantomData;

macro m() {
    // <[$ty]>::into_vec::<_>(Box::new([]));
    let _: Vec<()> = <[_]>::into_vec(Box::new([]));
    let _: Vec<usize> = <[_]>::into_vec(Box::new([]));

    // <$ty as From<_>>::from($expr);
    let _ = String::from("str");
    let _ = u8::from(false);

    // TEST: Write infer generics corresponding to trait method.
    trait Parser: Sized {
        fn parse_from<I, T>(itr: I) -> Self
        where
            I: IntoIterator<Item = T>,
            T: Into<String>;
    }
    impl<T: Parser> Parser for Box<T> {
        fn parse_from<I, It>(itr: I) -> Self
        where
            I: IntoIterator<Item = It>,
            It: Into<String>,
        {
            Box::new(<T as Parser>::parse_from(itr))
        }
    }

    // TEST: Write infer args for generics with defaults, to satisfy generic contexts.
    enum Entry<'a, T, A = Global>
    where
        A: Allocator,
    {
        Occupied(PhantomData<&'a (T, A)>),
        Vacant(PhantomData<&'a (T, A)>),
    }
    impl<'a, T, A> Entry<'a, T, A>
    where
        A: Allocator,
    {
        fn dummy(self, _default: T) {
            match self {
                Entry::Occupied(_entry) => {}
                Entry::Vacant(_entry) => {}
            }
        }
    }

    // TEST: Avoid writing infer generics in contexts where it cannot be used (e.g. generic traits in impl headers).
    struct S;
    impl PartialEq for S {
        fn eq(&self, other: &Self) -> bool { true }
    }
}

#[test]
fn test() {
    m!();
}
