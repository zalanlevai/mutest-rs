//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    // <[$ty]>::into_vec::<_>(Box::new([]));
    let _: Vec<()> = <[_]>::into_vec(Box::new([]));
    let _: Vec<usize> = <[_]>::into_vec(Box::new([]));

    // <$ty as From<_>>::from($expr);
    let _ = String::from("str");
    let _ = u8::from(false);

    // TEST: Write infer generics corresponding to trait method.
    pub trait Parser: Sized {
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
}

#[test]
fn test() {
    m!();
}
