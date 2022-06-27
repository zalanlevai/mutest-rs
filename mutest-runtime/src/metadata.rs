pub use phf::phf_map as static_map;

pub type TestPath = &'static str;

#[derive(Debug)]
pub struct SubstMeta {
    pub mutation: &'static MutationMeta,
}

#[derive(Debug)]
pub struct MutationMeta {
    pub id: u32,
    pub display_name: &'static str,
    pub display_location: &'static str,
    pub reachable_from: phf::Map<TestPath, usize>,
    pub undetected_diagnostic: &'static str,
}

#[derive(Debug)]
pub struct MutantMeta<S: 'static> {
    pub mutations: &'static [&'static MutationMeta],
    pub substitutions: S,
}
