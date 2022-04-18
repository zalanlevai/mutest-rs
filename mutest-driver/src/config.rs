use std::path::PathBuf;

use mutest_emit::codegen::mutation::Operators;

pub enum Mode {
    PrintMutationTargets,
    PrintCode,
    Build,
    BuildAndRun(Vec<String>),
}

pub struct Options<'op, 'm> {
    pub mode: Mode,
    pub operators: Operators<'op, 'm>,
    pub mutation_depth: usize,
    pub mutant_max_mutations_count: usize,
}

pub struct Config<'op, 'm> {
    pub package_directory_path: PathBuf,
    pub crate_root_path: Option<PathBuf>,
    pub opts: Options<'op, 'm>,
}

impl<'op, 'm> Config<'op, 'm> {
    pub fn crate_root_path(&self) -> PathBuf {
        self.crate_root_path.clone().unwrap_or(self.package_directory_path.join("src/main.rs"))
    }
}
