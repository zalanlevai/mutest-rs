pub struct SubstMeta<'a> {
    pub mutation: &'a MutationMeta,
}

pub struct MutationMeta {

}

pub struct MutantMeta<'a, S> {
    // pub mutations: Vec<&'a MutationMeta>,
    pub mutations: &'a [&'a MutationMeta],
    pub substitutions: S,
}
