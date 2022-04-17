pub struct SubstMeta<'a> {
    pub mutation: &'a MutationMeta<'a>,
}

pub struct MutationMeta<'a> {
    pub display_name: &'a str,
    pub display_location: &'a str,
}

pub struct MutantMeta<'a, S> {
    pub mutations: &'a [&'a MutationMeta<'a>],
    pub substitutions: S,

    pub undetected_diagnostic: &'a str,
}
