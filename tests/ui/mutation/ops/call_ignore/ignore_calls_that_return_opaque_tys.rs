//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete, call_value_default_shadow

fn ascii_letters(uppercase: bool) -> impl Iterator<Item = char> {
    match uppercase {
        true => 'A'..'Z',
        false => 'a'..'z',
    }
}

fn f() {
    let _ = ascii_letters(true);
}

#[test]
fn test() {
    f();
}
