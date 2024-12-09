//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![feature(async_closure)]

#![allow(unused)]

use std::future::Future;

struct Vec3 { x: f32, y: f32, z: f32 }

// TEST: Empty async function with no parameters and no explicit return type.
async fn f_empty() {}

// TEST: Empty async function with explicit return type.
async fn f_explicit_return_ty() -> () {}

// TEST: Empty async function with `impl Trait` return type.
async fn f_impl_trait_return_ty() -> impl Into<()> {}

// TEST: Async function with only an implicit return expression.
async fn f_implicit_return_expr() -> Result<(), String> {
    Ok(())
}

// TEST: Async function with simple parameters.
async fn f_simple_params(a: u32, b: &str, c: &mut Vec3) {}

// TEST: Async function with parameters with complex patterns.
async fn f_complex_params(a: u32, (b, c): &(i8, ()), Vec3 { x, y, z }: &mut Vec3) {}

// TEST: Async function declarations without bodies.
trait AsyncTrait {
    async fn f_empty();
    async fn f_with_params_and_return_ty(a: u32, b: &str, c: &mut Vec3) -> impl Into<String>;
}

// TEST: Async function with simple expression body.
async fn f_zero() -> u8 { 0 }

// TEST: Async function with let and assignment statements.
async fn f_locals() {
    let one = 1.0;
    let mut vec = Vec3 { x: 0.0, y: 0.0, z: 0.0 };
    vec.x = one;
    vec.y += one;
    vec.y *= one;
}

// TEST: Async function with single await expression.
async fn f_one_await() {
    let _ = f_empty().await;
}

// TEST: Async function with multiple await expressions.
async fn f_multiple_awaits() {
    let _ = f_empty().await;
    let _ = f_implicit_return_expr().await;
}

// TEST: Async block.
fn f_async_block() -> impl Future<Output = u8> {
    async {
        let x = f_zero().await;
        x + 5
    }
}

// TEST: Async closures.
fn f_async_closures() {
    // TEST: Empty async closure with block body.
    let _ = async move || {};

    // TEST: Empty async closure with expression body.
    let _ = async move || ();

    // TEST: Empty async closure with explicit return type.
    let _ = async move || -> () {};

    // TEST: Async closure with complex parameters and await expressions.
    let _ = async move |a: u32, (b, _c): &(i8, ()), Vec3 { x, y, z }: &mut Vec3| -> i32 {
        let mut v = f_async_block().await as i32;
        v += a as i32;
        v -= *b as i32;
        v * ((*x * *y * *z) as i32)
    };
}
