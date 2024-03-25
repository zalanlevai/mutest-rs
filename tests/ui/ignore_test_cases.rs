//@ print-targets
//@ stdout
//@ mutest-flags: -v

fn tested_fn() {}

fn ignored_fn() {}

#[cfg(test)]
mod tests {
    #[test]
    fn active_test() {
        super::tested_fn();
    }

    #[test]
    #[ignore]
    fn ignored_test() {
        super::ignored_fn();
    }
}
