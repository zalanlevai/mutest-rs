use std::collections::HashMap;

use mutest_json::{DefId, Span};
use mutest_json::mutations::{MutationSafety, TargetId};

use crate::html::mutations::SubstHtml;
use crate::source_file::LineNo;

pub struct Definition {
    pub def_id: DefId,
    pub name: Option<String>,
    pub def_path: String,
    pub def_path_html: String,
    pub span: Option<Span>,
    pub nudge_prefix_lines: usize,
}

impl Definition {
    pub fn display_start_line(&self) -> Option<LineNo> {
        self.span.as_ref().map(|span| LineNo((span.begin.0 - self.nudge_prefix_lines) as u32))
    }
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
