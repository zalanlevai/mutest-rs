pub use phf::phf_map as static_map;

pub type TestPath = &'static str;

pub trait SubstMap: Sized + Clone {}

#[derive(Clone, Copy, Debug)]
pub struct SubstMeta {
    pub mutation: &'static MutationMeta,
}

#[derive(Debug)]
pub enum MutationSafety {
    Safe,
    Tainted,
    Unsafe,
}

#[derive(Debug)]
pub struct MutationMeta {
    pub id: u32,
    pub safety: MutationSafety,
    pub op_name: &'static str,
    pub display_name: &'static str,
    pub display_location: &'static str,
    pub reachable_from: phf::Map<TestPath, usize>,
    pub undetected_diagnostic: &'static str,
}

#[derive(Debug)]
pub struct MutantMeta<S: SubstMap + 'static> {
    pub id: u32,
    pub mutations: &'static [&'static MutationMeta],
    pub substitutions: &'static S,
}

impl<S: SubstMap> MutantMeta<S> {
    pub fn is_unsafe(&self) -> bool {
        self.mutations.iter().any(|m| !matches!(m.safety, MutationSafety::Safe))
    }
}
