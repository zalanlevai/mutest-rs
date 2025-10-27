//@ print-tests
//@ print-targets
//@ print-mutations
//@ run: fail
//@ stdout
//@ mutest-flags: --Zembedded

// HACK: Stub out the runtime dependencies of the embedded_test macro expansion.
mod embedded_test {
    pub mod export {
        pub fn check_outcome((): ()) -> ! {
            std::process::exit(0);
        }
    }
}

struct Peripherals;

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

mod tests {
    use super::*;

    fn init() -> Peripherals {
        Peripherals
    }

    fn simple_test() {
        assert_eq!(5, mutable_fn(2, 3));
    }
    #[doc(hidden)]
    #[allow(unused)]
    fn __simple_test_entrypoint() -> ! {
        {
            let outcome;
            { let state = init(); outcome = simple_test(); }
            embedded_test::export::check_outcome(outcome);
        }
    }
    #[link_section = ".embedded_test.tests"]
    #[export_name = "{\"disambiguator\":10289733635752023323,\"name\":\"simple_test\",\"ignored\":false,\"should_panic\":false}"]
    static __SIMPLE_TEST_SYM: (fn() -> !, &'static str) = (__simple_test_entrypoint, "test_suite::tests");

    fn test_taking_peripherals(_peripherals: Peripherals) {
        assert_eq!(5, mutable_fn(2, 3));
    }
    #[doc(hidden)]
    #[allow(unused)]
    fn __test_taking_peripherals_entrypoint() -> ! {
        {
            let outcome;
            { let state = init(); outcome = test_taking_peripherals(state); }
            embedded_test::export::check_outcome(outcome);
        }
    }
    #[link_section = ".embedded_test.tests"]
    #[export_name = "{\"disambiguator\":10289733635752023323,\"name\":\"test_taking_peripherals\",\"ignored\":false,\"should_panic\":false}"]
    static __TEST_TAKING_PERIPHERALS_SYM: (fn() -> !, &'static str) = (__test_taking_peripherals_entrypoint, "test_suite::tests");

    fn panicking_test() {
        panic!();
    }
    #[doc(hidden)]
    #[allow(unused)]
    fn __panicking_test_entrypoint() -> ! {
        {
            let outcome;
            { let state = init(); outcome = panicking_test(); }
            embedded_test::export::check_outcome(outcome);
        }
    }
    #[link_section = ".embedded_test.tests"]
    #[export_name = "{\"disambiguator\":10289733635752023323,\"name\":\"panicking_test\",\"ignored\":false,\"should_panic\":true}"]
    static __PANICKING_TEST_SYM: (fn() -> !, &'static str) = (__panicking_test_entrypoint, "test_suite::tests");

    fn panicking_test_with_message() {
        panic!("message");
    }
    #[doc(hidden)]
    #[allow(unused)]
    fn __panicking_test_with_message_entrypoint() -> ! {
        {
            let outcome;
            { let state = init(); outcome = panicking_test_with_message(); }
            embedded_test::export::check_outcome(outcome);
        }
    }
    #[link_section = ".embedded_test.tests"]
    #[export_name = "{\"disambiguator\":10289733635752023323,\"name\":\"panicking_test_with_message\",\"ignored\":false,\"should_panic\":true}"]
    static __PANICKING_TEST_WITH_MESSAGE_SYM: (fn() -> !, &'static str) = (__panicking_test_with_message_entrypoint, "test_suite::tests");

    fn ignored_test() {}
    #[doc(hidden)]
    #[allow(unused)]
    fn __ignored_test_entrypoint() -> ! {
        {
            let outcome;
            { let state = init(); outcome = ignored_test(); }
            embedded_test::export::check_outcome(outcome);
        }
    }
    #[link_section = ".embedded_test.tests"]
    #[export_name = "{\"disambiguator\":10289733635752023323,\"name\":\"ignored_test\",\"ignored\":true,\"should_panic\":false}"]
    static __IGNORED_TEST_SYM: (fn() -> !, &'static str) = (__ignored_test_entrypoint, "test_suite::tests");
}
