//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    #[test]
    fn test() {
        let _: Vec<()> = vec![];
        let _: Vec<usize> = vec![1, 2, 3];

        enum E {
            A,
        }
        let _: Vec<E> = vec![E::A];

        let _: Vec<Vec<&str>> = vec![vec!["a"], vec!["b"], vec!["c"], vec!["d"], vec!["e"]];
    }
}

m!();
