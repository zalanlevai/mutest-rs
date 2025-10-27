//@ ignore

struct Peripherals;

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[embedded_test::tests]
mod tests {
    use super::*;

    #[init]
    fn init() -> Peripherals {
        Peripherals
    }

    #[test]
    fn simple_test() {
        assert_eq!(5, mutable_fn(2, 3));
    }

    #[test]
    fn test_taking_peripherals(_peripherals: Peripherals) {
        assert_eq!(5, mutable_fn(2, 3));
    }

    #[test]
    #[should_panic]
    fn panicking_test() {
        panic!();
    }

    #[test]
    #[should_panic = "message"]
    fn panicking_test_with_message() {
        panic!("message");
    }

    #[test]
    #[ignore]
    fn ignored_test() {}
}
