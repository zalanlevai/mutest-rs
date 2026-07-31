//@ build
//@ stderr: empty

mod alloc {
    extern crate alloc;
}

#[test]
fn test() {
    let _ = Vec::<()>::new();
    let _: String = "string".to_owned();
}
