use std::path::PathBuf;

pub struct Options {
    pub port: u16,
}

pub struct Config {
    pub json_dir_path: PathBuf,
    pub opts: Options,
}
