#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;

fn main() {
    println!("cargo:rustc-env=RUSTC_VERSION_STR={}", rustc_interface::util::rustc_version_str().unwrap_or("unknown"));
}
