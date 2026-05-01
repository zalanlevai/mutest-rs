use std::path::PathBuf;

pub struct Options {
    pub port: u16,
    pub open: bool,
}

pub struct Config {
    pub json_root_dir_path: PathBuf,
    pub opts: Options,
}
