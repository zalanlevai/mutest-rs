//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(async_closure)]
#![feature(decl_macro)]

#![allow(unused)]

use std::future::Future;

struct Vec3 { x: f32, y: f32, z: f32 }

macro info($fmt:expr, $($args:tt)*) {
    println!("[info] {}", format_args!($fmt, $($args)*));
}

macro m() {
    async fn f_zero() -> u8 { 0 }

    fn f_async_block() -> impl Future<Output = u8> {
        async {
            let x = f_zero().await;
            x + 5
        }
    }

    async fn f_nonsense(a: u32, (b, _c): &(i8, ()), Vec3 { x, y, z }: &mut Vec3) -> i32 {
        let mut v = f_async_block().await as i32;
        v += a as i32;
        v -= *b as i32;
        v * ((*x * *y * *z) as i32)
    }

    fn f_async_closure_nonsense() {
        let _ = async move |(a, b): (u32, i8), Vec3 { x, y, z }: &mut Vec3| -> i32 {
            let mut v = f_async_block().await as i32;
            info!("v = {}", v);
            *x *= (a + b as u32) as f32;
            *y *= (a + b as u32) as f32;
            *z *= (a + b as u32) as f32;
            v
        };
    }
}

async fn f_zero() {}
fn f_async_block() {}
async fn f_nonsense() {}
fn f_async_closure_nonsense() {}

m!();
