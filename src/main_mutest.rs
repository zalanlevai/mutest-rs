pub struct Mutant<'a> {
    /// /Users/zalanlevai/Developer/mutest-test/src/foo.rs:1:12: 1:18 (#0)
    pub r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6MToxMjogMToxOCAoIzAp: Option<&'a dyn std::any::Any>,
    /// /Users/zalanlevai/Developer/mutest-test/src/foo.rs:2:5: 6:6 (#0)
    pub r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6Mjo1OiA2OjYgKCMwKQ: Option<&'a dyn std::any::Any>,
    /// /Users/zalanlevai/Developer/mutest-test/src/foo.rs:2:8: 2:18 (#4)
    pub r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6Mjo4OiAyOjE4ICgjNCk: Option<&'a dyn std::any::Any>,
    /// /Users/zalanlevai/Developer/mutest-test/src/foo.rs:3:9: 3:14 (#0)
    pub r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6Mzo5OiAzOjE0ICgjMCk: Option<&'a dyn std::any::Any>,
    /// /Users/zalanlevai/Developer/mutest-test/src/foo.rs:5:9: 5:14 (#0)
    pub r#L1VzZXJzL3phbGFubGV2YWkvRGV2ZWxvcGVyL211dGVzdC10ZXN0L3NyYy9mb28ucnM6NTo5OiA1OjE0ICgjMCk: Option<&'a dyn std::any::Any>,
}

pub const MUTEST_MUTANT: std::cell::RefCell<Option<Mutant>> = std::cell::RefCell::new(None);

#[main]
fn main() {
    extern crate mutest_mutations;
    extern crate mutest_runtime;

    mutest_runtime::test_main_static::<Mutant>(&[
        &include!("../mutants/Ou0zFa.rs"),
    ]);
}
