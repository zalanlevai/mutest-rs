//@ build
//@ stderr: empty

#![allow(unused)]

fn counter() -> Vec<u32> {
    vec![1, 2]
}

struct Counter;

impl Counter {
    fn new() -> Self { Counter }
}

fn total() -> usize {
    let counter = counter();
    let other = self::counter();
    counter.len() + other.len()
}

#[allow(non_camel_case_types)]
fn generic<Counter: Default>() -> Counter {
    let _ = self::Counter::new();
    Counter::default()
}

#[test]
fn test() {
    assert_eq!(total(), 4);
    let _: u8 = generic();
}
