//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    match [1, 2, 3, 4, 5] {
        [one, two, three, four, five] => {}
        [one, two, three, four, five, ..] => {}
        [one, two, ..] => {}
        [one, two, .., five] => {}
        [one, two, .., four, five] => {}
        [..] => {}
        [.., five] => {}
        [.., four, five] => {}
    }
}
