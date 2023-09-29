use rustc_session::Session;

use crate::codegen::ast;
use crate::codegen::symbols::{DUMMY_SP, Ident, sym};

pub fn register(sess: &Session, krate: &mut ast::Crate) {
    let g = &sess.parse_sess.attr_id_generator;

    // #![feature(register_tool)]
    let feature_register_tool_attr = ast::mk::attr_inner(g, DUMMY_SP,
        Ident::new(sym::feature, DUMMY_SP),
        ast::mk::attr_args_delimited(DUMMY_SP, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
            ast::mk::tt_token_joint(DUMMY_SP, ast::TokenKind::Ident(sym::register_tool, false)),
        ])),
    );
    // #![register_tool(mutest)]
    let register_tool_mutest_attr = ast::mk::attr_inner(g, DUMMY_SP,
        Ident::new(sym::register_tool, DUMMY_SP),
        ast::mk::attr_args_delimited(DUMMY_SP, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
            ast::mk::tt_token_joint(DUMMY_SP, ast::TokenKind::Ident(*sym::mutest, false)),
        ])),
    );

    krate.attrs.push(feature_register_tool_attr);
    krate.attrs.push(register_tool_mutest_attr);
}

pub fn ignore<'tcx, I>(attrs: I) -> bool
where
    I: IntoIterator<Item = &'tcx ast::Attribute>,
{
    attrs.into_iter().any(|attr| ast::inspect::is_word_attr(attr, Some(*sym::mutest), sym::ignore))
}

pub fn skip<'tcx, I>(attrs: I) -> bool
where
    I: IntoIterator<Item = &'tcx ast::Attribute>,
{
    attrs.into_iter().any(|attr| ast::inspect::is_word_attr(attr, Some(*sym::mutest), sym::skip))
}
