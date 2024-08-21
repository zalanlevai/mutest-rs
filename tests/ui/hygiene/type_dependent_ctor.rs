//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

pub(crate) type Result<T, E = ()> = ::std::result::Result<T, E>;

macro m() {
    #[allow(unused)]
    fn f() -> crate::Result<()> {
        crate::Result::Ok(())
    }
}

m!();
