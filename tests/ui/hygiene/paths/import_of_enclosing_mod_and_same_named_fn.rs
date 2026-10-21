//@ build
//@ stderr: empty

#![allow(unused)]

mod store {
    mod items {
        // NOTE: Imports both the enclosing `items` module and the `items` function.
        use super::items;

        pub fn count() -> usize {
            items().len()
        }
    }

    pub fn items() -> Vec<u32> {
        vec![1, 2]
    }

    pub use items::count;
}

#[test]
fn test() {
    assert_eq!(store::count(), 2);
}
