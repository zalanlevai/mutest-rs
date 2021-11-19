pub fn foo(n: u32) -> u32 {
    extern crate mutest_runtime;

    mutest_runtime::local!(r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6MToxMjogMToxOCAoIzAp, n, u32);

    mutest_runtime::expr!(r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6Mjo1OiA2OjYgKCMwKQ, {
        if mutest_runtime::expr!(r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6Mjo4OiAyOjE4ICgjNCk, n % 2 == 0) {
            mutest_runtime::expr!(r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6Mzo5OiAzOjE0ICgjMCk, n * 2)
        } else {
            mutest_runtime::expr!(r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6NTo5OiA1OjE0ICgjMCk, n - 1)
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(foo(5), 4)
    }
}
