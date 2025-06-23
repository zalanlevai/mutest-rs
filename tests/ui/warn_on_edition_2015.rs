//@ print-mutants
//@ build
//@ stdout
//@ stderr
//@ edition: 2015

fn mutated_fn(a: i32, b: i32) -> i32 {
    a + b
}

#[test]
fn test() {
    assert_eq!(11, mutated_fn(5, 6));
}
