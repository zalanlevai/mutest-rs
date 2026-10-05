use std::iter;

use itertools::Itertools;
use rustc_data_structures::smallvec::{SmallVec, smallvec};
use rustc_data_structures::thin_vec::ThinVec;
use rustc_middle::ty::TyCtxt;
use rustc_session::Session;
use rustc_span::{ExpnData, LocalExpnId};

use crate::analysis::tests::Test;
use crate::codegen::ast;
use crate::codegen::ast::mut_visit::MutVisitor;
use crate::codegen::symbols::{ExpnKind, Ident, Span, Symbol, sym};
use crate::codegen::symbols::hygiene::AstPass;

pub trait TcxExpansionExt {
    fn expansion_for_ast_pass(
        &self,
        ast_pass: AstPass,
        call_site: Span,
        features: &[Symbol],
    ) -> LocalExpnId;
}

impl<'tcx> TcxExpansionExt for TyCtxt<'tcx> {
    fn expansion_for_ast_pass(
        &self,
        ast_pass: AstPass,
        call_site: Span,
        features: &[Symbol],
    ) -> LocalExpnId {
        let expn_data = ExpnData::allow_unstable(
            ExpnKind::AstPass(ast_pass),
            call_site,
            self.sess.edition(),
            features.into(),
            None,
            None,
        );
        self.with_stable_hashing_context(|hcx| LocalExpnId::fresh(expn_data, hcx))
    }
}

pub const GENERATED_CODE_PRELUDE: &str = r#"
#![allow(unused_features)]
#![allow(unused_imports)]
"#;

fn dedupe_extern_crate_decls(items: &mut ThinVec<Box<ast::Item>>, sym: Symbol) {
    if let Some((first_extern_crate_index, _)) = items.iter().find_position(|&item| ast::inspect::is_extern_crate_decl(item, sym)) {
        let mut i = first_extern_crate_index + 1;
        while let Some(item) = items.get(i) {
            if !ast::inspect::is_extern_crate_decl(item, sym) {
                i += 1;
                continue;
            }

            items.remove(i);
        }
    }
}

fn ensure_test_scope(items: &mut ThinVec<Box<ast::Item>>) {
    dedupe_extern_crate_decls(items, sym::test)
}

struct TestCaseCleaner<'tcx, 'tst> {
    sess: &'tcx Session,
    tests: &'tst [Test],
}

impl<'tcx, 'tst> ast::mut_visit::MutVisitor for TestCaseCleaner<'tcx, 'tst> {
    fn visit_crate(&mut self, krate: &mut ast::Crate) {
        ast::mut_visit::walk_crate(self, krate);

        ensure_test_scope(&mut krate.items);
    }

    fn flat_map_item(&mut self, mut item: Box<ast::Item>) -> SmallVec<[Box<ast::Item>; 1]> {
        if let ast::ItemKind::Mod(..) = item.kind {
            ast::mut_visit::walk_item(self, &mut item);

            if let ast::ItemKind::Mod(_, _, ast::ModKind::Loaded(ref mut items, _, _)) = item.kind {
                ensure_test_scope(items);
            }
        }

        if let Some(_test) = self.tests.iter().find(|&test| test.descriptor.id == item.id) {
            return smallvec![];
        }

        if let Some(_test) = self.tests.iter().find(|&test| test.item.id == item.id) {
            let g = &self.sess.psess.attr_id_generator;

            // #[test]
            let test_attr = ast::mk::attr_outer(g, item.span, ast::Safety::Default, Ident::new(sym::test, item.span), ast::AttrArgs::Empty);

            item.attrs = item.attrs.into_iter()
                .filter(|attr| !attr.has_name(sym::rustc_test_marker))
                .filter(|attr| !attr.has_name(sym::test))
                .chain(iter::once(test_attr))
                .collect();
        }

        smallvec![item]
    }
}

pub fn clean_up_test_cases(sess: &Session, tests: &[Test], krate: &mut ast::Crate) {
    let mut cleaner = TestCaseCleaner { sess, tests };
    cleaner.visit_crate(krate);
}
