//@ fail
//@ stderr

fn f() {
    let _: bool = !Default::default();
}

#[test]
fn test() {
    f();
}
