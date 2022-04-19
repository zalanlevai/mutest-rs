#[derive(Debug)]
pub struct SubstMeta<'a> {
    pub mutation: &'a MutationMeta<'a>,
}

#[derive(Debug)]
pub struct MutationMeta<'a> {
    pub id: u32,
    pub display_name: &'a str,
    pub display_location: &'a str,
}

#[derive(Debug)]
pub struct MutantMeta<'a, S> {
    pub mutations: &'a [&'a MutationMeta<'a>],
    pub substitutions: S,

    pub undetected_diagnostic: &'a str,
}
