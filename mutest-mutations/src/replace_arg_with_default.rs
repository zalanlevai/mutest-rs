use std::borrow::Cow;

#[derive(Debug)]
pub struct ReplaceArgWithDefault<'a> {
    pub param_ident: Cow<'a, str>,
}

impl mutest_bridge::Mutation for ReplaceArgWithDefault<'_> {}
