pub fn foo(n: u32) -> u32 {
    if n % 2 == 0 {
        n * 2
    } else {
        n - 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(foo(5), 4)
    }
}
