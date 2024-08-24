#![crate_type = "lib"]

extern crate dummy_crate;

pub mod reexport {
    pub use dummy_crate::{foo as dummy_foo};
}
