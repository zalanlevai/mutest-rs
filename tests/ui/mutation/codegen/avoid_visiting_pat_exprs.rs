//@ build
//@ stderr: empty

fn mutable_fn_with_pat_exprs(c @ ('Z' | _): char) {
    if let 'Z' | 'z' = c {}

    match c {
        'Z' | 'z' => {}
        'a' => {}
        _ => {}
    }
}

fn mutable_fn_with_range_pats(name: &str) -> bool {
    if let b'a'..=b'i' | b'k'..=b'y' | b'A'..=b'I' | b'K'..=b'Y' = name.as_bytes()[0] {
        return true;
    }
    false
}

#[test]
fn test() {
    mutable_fn_with_pat_exprs('Z');
    mutable_fn_with_range_pats("abc");
}
