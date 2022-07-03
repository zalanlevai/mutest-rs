use crate::codegen::ast;
use crate::codegen::symbols::{DUMMY_SP, Ident, sym};

pub fn register(krate: &mut ast::Crate) {
    // #![feature(register_tool)]
    let feature_register_tool_attr = ast::attr::mk_attr_inner(ast::attr::mk_list_item(
        Ident::new(sym::feature, DUMMY_SP),
        vec![ast::attr::mk_nested_word_item(Ident::new(sym::register_tool, DUMMY_SP))],
    ));
    // #![register_tool(mutest)]
    let register_tool_mutest_attr = ast::attr::mk_attr_inner(ast::attr::mk_list_item(
        Ident::new(sym::register_tool, DUMMY_SP),
        vec![ast::attr::mk_nested_word_item(Ident::new(*sym::mutest, DUMMY_SP))],
    ));

    krate.attrs.push(feature_register_tool_attr);
    krate.attrs.push(register_tool_mutest_attr);
}

pub fn skip<'tcx, I>(attrs: I) -> bool
where
    I: IntoIterator<Item = &'tcx ast::Attribute>,
{
    attrs.into_iter().any(|attr| ast::inspect::is_word_attr(attr, Some(*sym::mutest), sym::skip))
}
