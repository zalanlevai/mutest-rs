//@ build
//@ stderr: empty

#![feature(decl_macro)]

macro m() {
    let _: Vec<()> = vec![];
    let _: Vec<usize> = vec![1, 2, 3];

    enum E {
        A,
    }
    let _: Vec<E> = vec![E::A];

    let _: Vec<Vec<&str>> = vec![vec!["a"], vec!["b"], vec!["c"], vec!["d"], vec!["e"]];
}

#[test]
fn test() {
    m!();
}
