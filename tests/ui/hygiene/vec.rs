//@ build
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    #[test]
    fn test() {
        let _: Vec<()> = vec![];
        let _: Vec<usize> = vec![1, 2, 3];
    }
}

m!();
