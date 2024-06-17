//@ build
//@ stderr: empty
//@ aux-build: dummy_crate.rs
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro extern_crate_hygiene() {
    extern crate dummy_crate;
    extern crate dummy_crate as dummy;

    dummy_crate::foo();
    dummy::bar();
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_extern_crates() {
        mod dummy_crate {}
        mod dummy {}

        crate::extern_crate_hygiene!();
    }
}
