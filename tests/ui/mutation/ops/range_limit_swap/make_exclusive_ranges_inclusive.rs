//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: range_limit_swap

fn f() {
    let arr = [0, 1, 2, 3, 4];

    for _ in 0..5 {}
    for _ in &arr[..3] {}

    match (&arr[..3], &arr[1..3]) {
        (_, _) => {}
    }

    if arr[..3].len() == arr[1..3].len() {}

    fn g<T>(_: &[T]) {}

    g(&arr[..3]);
    g(&arr[1..3]);
}

#[test]
fn test() {
    f();
}
