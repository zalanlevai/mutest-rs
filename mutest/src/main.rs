use std::env;
use std::path::PathBuf;

use mutest_driver::config::Config;

fn main() {
    let package_directory_path = env::args().nth(1).map(PathBuf::from).expect("Usage: mutest <path>");

    let config = Config {
        package_directory_path,
    };

    mutest_driver::main(config).unwrap();
}
