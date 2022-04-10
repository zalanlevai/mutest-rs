use rustc_expand::base::ExtCtxt;
use rustc_hash::FxHashMap;

use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::mut_visit::MutVisitor;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, path, sym};
use crate::codegen::symbols::hygiene::AstPass;
use crate::codegen::mutation::{Mutant, MutId, Subst, SubstLoc};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct SubstLocId(SubstLoc);

impl SubstLoc {
    pub fn into_subst_loc_id(&self) -> SubstLocId {
        SubstLocId(*self)
    }
}

impl SubstLocId {
    pub fn into_symbol_name(&self) -> String {
        match self.0 {
            SubstLoc::InsertBefore(node_id) => format!("subst_bef_{}", node_id),
            SubstLoc::InsertAfter(node_id) => format!("subst_aft_{}", node_id),
            SubstLoc::Replace(node_id) => format!("subst_rep_{}", node_id),
        }
    }

    pub fn into_symbol(&self) -> Symbol {
        Symbol::intern(&self.into_symbol_name())
    }
}

pub fn expand_subst_match_expr(sp: Span, subst_loc: SubstLoc, original: Option<P<ast::Expr>>, substs: &Vec<(MutId, &Subst)>) -> P<ast::Expr> {
    let mut arms = substs.into_iter()
        .map(|(mut_id, subst)| {
            // Some(subst) if ptr::eq(subst.mutation, &crate::mutest_generated::mutations::$mut_id) => $subst,
            let subst_ident = Ident::new(Symbol::intern("subst"), sp);
            let pat_some_subst = ast::mk::pat_tuple_struct(sp, path::Some(sp), vec![ast::mk::pat_ident(sp, subst_ident)]);
            let guard = ast::mk::expr_call_path(sp, path::ptr_eq(sp), vec![
                ast::mk::expr_field(sp, ast::mk::expr_ident(sp, subst_ident), Ident::new(*sym::mutation, sp)),
                ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp, path::mutations(sp), vec![Ident::new(mut_id.into_symbol(), sp)]))),
            ]);
            ast::mk::arm(sp, pat_some_subst, Some(guard), match subst {
                Subst::AstExpr(expr) => P(expr.clone()),
                Subst::AstStmt(stmt) => ast::mk::expr_block(ast::mk::block(sp, vec![stmt.clone()])),
            })
        })
        .collect::<Vec<_>>();

    // Some(_) => { unreachable!() }
    let unreachable = ast::mk::expr_block(ast::mk::block(sp, vec![
        ast::mk::stmt(sp, ast::StmtKind::MacCall(P(ast::MacCallStmt {
            attrs: ast::AttrVec::new(),
            mac: ast::MacCall {
                path: ast::mk::path_ident(sp, Ident::new(sym::unreachable, sp)),
                args: P(ast::MacArgs::Delimited(
                    ast::DelimSpan::from_single(sp),
                    ast::MacDelimiter::Parenthesis,
                    ast::mk::token_stream(vec![]),
                )),
                prior_type_ascription: None,
            },
            style: ast::MacStmtStyle::Semicolon,
            tokens: None,
        }))),
    ]));
    let pat_some_wild = ast::mk::pat_tuple_struct(sp, path::Some(sp), vec![ast::mk::pat_wild(sp)]);
    arms.push(ast::mk::arm(sp, pat_some_wild, None, unreachable));

    // None => $expr
    let pat_none = ast::mk::pat_path(sp, path::None(sp));
    arms.push(ast::mk::arm(sp, pat_none, None, match original {
        Some(expr) => expr,
        None => ast::mk::expr_noop(sp),
    }));

    // crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow()
    let borrow = Ident::new(*sym::borrow, sp);
    let mutant_handle_borrow_expr = ast::mk::expr_method_call_path_ident(sp, path::ACTIVE_MUTANT_HANDLE(sp), borrow, vec![]);
    // m.substitutions.$subst_loc_id.as_ref()
    let mutant_lookup_expr = ast::mk::expr_method_call(sp,
        ast::mk::expr_field_deep(sp,
            ast::mk::expr_ident(sp, Ident::new(*sym::mutant, sp)),
            vec![
                Ident::new(*sym::substitutions, sp),
                Ident::new(subst_loc.into_subst_loc_id().into_symbol(), sp),
            ]
        ),
        ast::mk::path_segment(sp, Ident::new(*sym::as_ref, sp), vec![]),
        vec![],
    );
    // crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow().and_then(|m| m.substitutions.$subst_loc_id.as_ref())
    let subst_lookup_expr = ast::mk::expr_method_call(sp,
        mutant_handle_borrow_expr,
        ast::mk::path_segment(sp, Ident::new(*sym::and_then, sp), vec![]),
        vec![ast::mk::expr_closure(sp, vec![Ident::new(*sym::mutant, sp)], mutant_lookup_expr)],
    );

    // match crate::mutest_generated::ACTIVE_MUTANT_HANDLE.borrow().and_then(|m| m.substitutions.$subst.as_ref()) { ... }
    ast::mk::expr_match(sp, subst_lookup_expr, arms)
}

pub fn expand_subst_match_stmt(sp: Span, subst_loc: SubstLoc, original: Option<ast::Stmt>, substs: &Vec<(MutId, &Subst)>) -> ast::Stmt {
    let original_expr = original.map(|v| ast::mk::expr_block(ast::mk::block(sp, vec![v])));
    ast::mk::stmt_expr(expand_subst_match_expr(sp, subst_loc, original_expr, substs))
}

struct SubstWriter<'op> {
    pub substitutions: FxHashMap<SubstLoc, Vec<(MutId, &'op Subst)>>,
    pub def_site: Span,
}

impl<'op> ast::mut_visit::MutVisitor for SubstWriter<'op> {
    fn visit_block(&mut self, block: &mut P<ast::Block>) {
        ast::mut_visit::noop_visit_block(block, self);

        let mut i = 0;
        while i < block.stmts.len() {
            let stmt_id = block.stmts[i].id;

            let insert_before_loc = SubstLoc::InsertBefore(stmt_id);
            if let Some(before) = self.substitutions.get(&insert_before_loc) {
                block.stmts.insert(i, expand_subst_match_stmt(self.def_site, insert_before_loc, None, before));
                i += 1;
            }

            let replacement_loc = SubstLoc::Replace(stmt_id);
            if let Some(replacements) = self.substitutions.get(&replacement_loc) {
                let stmt = &mut block.stmts[i];
                *stmt = expand_subst_match_stmt(stmt.span, replacement_loc, Some(stmt.clone()), replacements);
            }

            let insert_after_loc = SubstLoc::InsertAfter(stmt_id);
            if let Some(after) = self.substitutions.get(&insert_after_loc) {
                i += 1;
                block.stmts.insert(i, expand_subst_match_stmt(self.def_site, insert_after_loc, None, after));
            }

            i += 1;
        }
    }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        ast::mut_visit::noop_visit_expr(expr, self);

        let insert_before_loc = SubstLoc::InsertBefore(expr.id);
        if let Some(_before) = self.substitutions.get(&insert_before_loc) {
            // TODO: Expand into a block expression, making place for insertions before and after.
            panic!("invalid substitution: substitutions cannot be inserted before expressions");
        }

        let replacement_loc = SubstLoc::Replace(expr.id);
        if let Some(replacements) = self.substitutions.get(&replacement_loc) {
            *expr = expand_subst_match_expr(expr.span, replacement_loc, Some(expr.clone()), replacements);
        }

        let insert_after_loc = SubstLoc::InsertAfter(expr.id);
        if let Some(_after) = self.substitutions.get(&insert_after_loc) {
            // TODO: Expand into a block expression, making place for insertions before and after.
            panic!("invalid substitution: substitutions cannot be inserted after expressions");
        }
    }
}

pub fn write_substitutions(ecx: &mut ExtCtxt<'_>, mutants: &Vec<Mutant>, krate: &mut ast::Crate) {
    let mut substitutions: FxHashMap<SubstLoc, Vec<(MutId, &Subst)>> = Default::default();
    for mutant in mutants {
        for mutation in &mutant.mutations {
            for subst in &mutation.substs {
                let substitution = (mutation.id, &subst.substitute);

                substitutions.entry(subst.location)
                    .and_modify(|substs| substs.push(substitution))
                    .or_insert_with(|| vec![substitution]);
            }
        }
    }

    let expn_id = ecx.resolver.expansion_for_ast_pass(
        DUMMY_SP,
        AstPass::TestHarness,
        &[sym::rustc_attrs],
        None,
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut subst_writer = SubstWriter { substitutions, def_site };
    subst_writer.visit_crate(krate);
}
