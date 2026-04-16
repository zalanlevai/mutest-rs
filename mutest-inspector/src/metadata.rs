use std::collections::HashMap;

use mutest_json::{DefId, Span};
use mutest_json::mutations::{MutationSafety, TargetId};

use crate::html::mutations::SubstHtml;

pub struct Definition {
    pub def_id: DefId,
    pub name: Option<String>,
    pub def_path: String,
    pub def_path_html: String,
    pub span: Option<Span>,
}

pub struct Test {
    pub def_id: DefId,
    pub span: Span,
    pub ignore: bool,
}

pub struct Target {
    pub def_id: DefId,
    pub reachable_from: HashMap<String, ()>,
}

pub struct Mutation {
    pub target_id: TargetId,
    pub origin_span: Span,
    pub mutation_op: String,
    pub display_name: String,
    pub display_name_html: String,
    pub subst_htmls: Vec<SubstHtml>,
    pub safety: MutationSafety,
}
