//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: range_limit_swap

fn f() {
    let arr = [0, 1, 2, 3, 4];

    for _ in 1.. {}
    for _ in &arr[..] {}

    match (&arr[2..], &arr[..]) {
        (_, _) => {}
    }

    if arr[2..].len() == arr[..].len() {}

    fn g<T>(_: &[T]) {}

    g(&arr[2..]);
    g(&arr[..]);
}

#[test]
fn test() {
    f();
}
