pub mod config;

use crate::config::Config;

pub fn run(config: Config) {
    println!("flashing test binary `{}` onto target", config.test_bin_path.display());
    todo!();
}
