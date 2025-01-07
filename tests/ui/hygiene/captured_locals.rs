//@ build
//@ stderr: empty

#![feature(decl_macro)]

#[test]
fn test() {
    let mut index = 0;
    assert_eq!(index, 0);

    macro set_index() {
        index = 1;
    }
    set_index!();
    assert_eq!(index, 1);

    macro m() {
        let mut index = 0;
        assert_eq!(index, 0);

        macro set_index() {
            index = 1;
        }
        set_index!();
        assert_eq!(index, 1);
    }
    m!();
}
