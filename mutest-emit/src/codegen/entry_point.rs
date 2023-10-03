use std::iter;

use rustc_middle::ty::TyCtxt;
use rustc_session::Session;
use smallvec::{SmallVec, smallvec};
use thin_vec::ThinVec;

use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::entry::EntryPointType;
use crate::codegen::ast::mut_visit::{ExpectOne, MutVisitor};
use crate::codegen::expansion::TcxExpansionExt;
use crate::codegen::symbols::{DUMMY_SP, Ident, Symbol, sym};
use crate::codegen::symbols::hygiene::AstPass;

fn entry_point_type(item: &ast::Item, depth: usize) -> EntryPointType {
    match item.kind {
        ast::ItemKind::Fn(..) => {
            if item.attrs.iter().any(|attr| attr.has_name(sym::start)) {
                EntryPointType::Start
            } else if item.attrs.iter().any(|attr| attr.has_name(sym::rustc_main)) {
                EntryPointType::RustcMainAttr
            } else if item.ident.name == sym::main {
                if depth == 0 {
                    EntryPointType::MainNamed
                } else {
                    EntryPointType::OtherMain
                }
            } else {
                EntryPointType::None
            }
        }
        _ => EntryPointType::None,
    }
}

fn is_allow_dead_code_attr(attr: &ast::Attribute) -> bool {
    // `#[allow(dead_code)]`
    attr.has_name(sym::allow) && attr.meta_item_list().is_some_and(|list| list.iter().any(|item| item.has_name(sym::dead_code)))
}

struct EntryPointCleaner<'tcx> {
    sess: &'tcx Session,
    depth: usize,
}

impl<'tcx> ast::mut_visit::MutVisitor for EntryPointCleaner<'tcx> {
    fn flat_map_item(&mut self, i: P<ast::Item>) -> SmallVec<[P<ast::Item>; 1]> {
        let g = &self.sess.parse_sess.attr_id_generator;

        self.depth += 1;
        let item = ast::mut_visit::noop_flat_map_item(i, self).expect_one("noop did something");
        self.depth -= 1;

        let mut item = item.into_inner();

        match entry_point_type(&item, self.depth) {
            // Retain items that are user-defined entry points as dead-code.
            EntryPointType::MainNamed | EntryPointType::Start => {
                if item.ident.name == sym::main {
                    item.ident = Ident::new(Symbol::intern("unused_main"), item.ident.span);
                }

                // #[allow(dead_code)]
                let allow_dead_code_attr = ast::mk::attr_outer(g, item.span,
                    Ident::new(sym::allow, item.span),
                    ast::mk::attr_args_delimited(item.span, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                        ast::mk::tt_token_joint(item.span, ast::token::TokenKind::Ident(sym::dead_code, false)),
                    ])),
                );

                item.attrs = item.attrs.into_iter()
                    .filter(|attr| !attr.has_name(sym::start))
                    // Remove any existing `#[allow(dead_code)]` attributes (e.g. those generated by the rustc test harness).
                    .filter(|attr| !is_allow_dead_code_attr(attr))
                    .chain(iter::once(allow_dead_code_attr))
                    .collect();
            }
            // Drop the entry point generated by the rustc test harness, which is marked with `#[rustc_main]`.
            EntryPointType::RustcMainAttr => return smallvec![],
            // Ignore items that are not entry points.
            EntryPointType::None | EntryPointType::OtherMain => {}
        };

        smallvec![P(item)]
    }
}

pub fn clean_entry_points(sess: &Session, krate: &mut ast::Crate) {
    let mut cleaner = EntryPointCleaner { sess, depth: 0 };
    cleaner.visit_crate(krate);
}

pub fn generate_dummy_main<'tcx>(tcx: TyCtxt<'tcx>, krate: &mut ast::Crate) {
    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::test, sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let def = def_site;

    // pub fn main {}
    let vis = ast::mk::vis_pub(def);
    let ident = Ident::new(sym::main, def);
    let body = ast::mk::block(def, ThinVec::new());
    let main = ast::mk::item_fn(def, vis, ident, None, None, ThinVec::new(), None, Some(body));

    krate.items.push(main);
}

pub fn remove_dummy_main(krate: &mut ast::Crate) {
    krate.items.retain(|item| item.ident.name != sym::main);
}