use std::path::PathBuf;

use crate::ctxt::TargetSpec;

pub enum OpenTarget {
    Workspace,
    Specific(String, Option<TargetSpec>),
}

pub struct Options {
    pub port: u16,
    pub open: Option<OpenTarget>,
}

pub struct Config {
    pub json_root_dir_path: PathBuf,
    pub opts: Options,
}
