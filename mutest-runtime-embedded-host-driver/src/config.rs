use std::path::PathBuf;

pub enum Mode {
    Simulate { mutation_id: u32 },
    Evaluate,
    Flakes { iterations_count: usize },
}

pub struct PrintOptions {
    pub detection_matrix: Option<()>,
    pub subsumption_matrix: Option<()>,
}

pub struct WriteOptions {
    pub out_dir: PathBuf,
    pub eval_stream: Option<()>,
}

pub struct Options {
    pub mode: Mode,
    pub verbosity: u8,
    pub report_timings: bool,
    pub print_opts: PrintOptions,
    pub write_opts: Option<WriteOptions>,
    pub exhaustive: bool,
}

pub struct Config {
    pub test_bin_path: PathBuf,

    pub opts: Options,
}
