//@ build
//@ stderr: empty

fn mutable_fn(c @ ('Z' | _): char) {
    if let 'Z' | 'z' = c {}

    match c {
        'Z' | 'z' => {}
        'a' => {}
        _ => {}
    }
}

#[test]
fn test() {
    mutable_fn('Z');
}
