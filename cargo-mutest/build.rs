use std::fs;

mod rust_toolchain_file {
    use serde::Deserialize;

    #[derive(Deserialize)]
    pub struct Toolchain {
        pub channel: String,
    }

    #[derive(Deserialize)]
    pub struct RustToolchainFile {
        pub toolchain: Toolchain,
    }

    impl RustToolchainFile {
        pub fn from_str(s: &str) -> Result<Self, toml::de::Error> {
            toml::from_str(s)
        }
    }
}

use rust_toolchain_file::RustToolchainFile;

const RUST_TOOLCHAIN_FILE_PATH: &str = "../rust-toolchain.toml";

fn main() {
    println!("cargo:rerun-if-changed={RUST_TOOLCHAIN_FILE_PATH}");

    let rust_toolchain_file_str = fs::read_to_string(RUST_TOOLCHAIN_FILE_PATH).expect("cannot read `rust-toolchain.toml` file");
    let rust_toolchain_file = RustToolchainFile::from_str(&rust_toolchain_file_str).expect("unrecognized `rust-toolchain.toml` file");

    let rust_toolchain_version = rust_toolchain_file.toolchain.channel;
    println!("cargo:rustc-env=RUST_TOOLCHAIN_VERSION={rust_toolchain_version}");
}
