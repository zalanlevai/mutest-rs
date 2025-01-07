//@ print-targets
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v

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
        help_test();
        inner::help_test_inner();
        super::help_test_standalone();
        super::help_program();
    }
}
