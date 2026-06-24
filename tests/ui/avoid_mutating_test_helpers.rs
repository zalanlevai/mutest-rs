//@ print-targets
//@ stdout
//@ stderr: empty
//@ rustc-flags: --cfg feature="specific_test" --cfg feature="not_test_utils"
//@ mutest-flags: -v

#![allow(unexpected_cfgs)]

fn help_program() {}

#[cfg(test)]
fn help_test_standalone() {}

#[cfg(test)]
mod tests {
    fn help_test() {}

    mod inner {
        pub fn help_test_inner() {}
    }

    #[test]
    fn test1() {
        fn test1_impl() {}
        test1_impl();

        help_test();
        inner::help_test_inner();
        super::help_test_standalone();
        super::help_program();
    }
}

#[test]
fn test_standalone() {
    fn test_standalone_impl() {}
    test_standalone_impl();

    help_program();
}

#[cfg(any(test, feature = "test_utils"))]
mod test_utils {
    pub fn help_test() {}
}

#[cfg(all(test, feature = "specific_test"))]
mod specific_test_utils {
    pub fn help_test() {}
}

#[cfg(any(not(test), feature = "not_test_utils"))]
mod not_test_utils {
    pub fn help_program() {}
}

#[test]
fn test_helpers_behind_complex_cfgs() {
    test_utils::help_test();
    specific_test_utils::help_test();
    not_test_utils::help_program();
}
