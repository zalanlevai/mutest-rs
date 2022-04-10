use crate::Mutation;

pub struct Mutant<'a, S> {
    pub mutations: Vec<&'a dyn Mutation>,
    pub substitutions: S,
}
