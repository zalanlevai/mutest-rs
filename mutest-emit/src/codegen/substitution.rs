use rustc_hash::FxHashMap;
use rustc_middle::ty::TyCtxt;
use rustc_session::Session;
use thin_vec::{ThinVec, thin_vec};

use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::mut_visit::MutVisitor;
use crate::codegen::cancellation;
use crate::codegen::expansion::TcxExpansionExt;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, path, sym};
use crate::codegen::symbols::hygiene::AstPass;
use crate::codegen::mutation::{Mutant, MutId, Subst, SubstDef, SubstLoc};

pub fn conflicting_substs(a: &SubstDef, b: &SubstDef) -> bool {
    match (&a.substitute, &b.substitute) {
        (Subst::AstLocal(..), Subst::AstLocal(..)) => false,
        _ => a.location == b.location,
    }
}

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
            SubstLoc::InsertBefore(node_id, _) => format!("subst_bef_{}", node_id),
            SubstLoc::InsertAfter(node_id, _) => format!("subst_aft_{}", node_id),
            SubstLoc::Replace(node_id, _) => format!("subst_rep_{}", node_id),
        }
    }

    pub fn into_symbol(&self) -> Symbol {
        Symbol::intern(&self.into_symbol_name())
    }
}

fn mk_subst_match_expr(sp: Span, _subst_loc: SubstLoc, subst_loc_idx: usize, default: Option<P<ast::Expr>>, substs: Vec<(MutId, P<ast::Expr>)>) -> P<ast::Expr> {
    let mut arms = substs.into_iter()
        .map(|(mut_id, subst)| {
            // Some(subst) if subst.mutation.id == crate::mutest_generated::mutations::$mut_id.id => $subst,
            let subst_ident = Ident::new(Symbol::intern("subst"), sp);
            let pat_some_subst = ast::mk::pat_tuple_struct(sp, path::Some(sp), thin_vec![ast::mk::pat_ident(sp, subst_ident)]);
            let guard = ast::mk::expr_binary(sp, ast::BinOpKind::Eq,
                ast::mk::expr_field_deep(sp, ast::mk::expr_ident(sp, subst_ident), vec![Ident::new(*sym::mutation, sp), Ident::new(*sym::id, sp)]),
                ast::mk::expr_field(sp, ast::mk::expr_path(ast::mk::pathx(sp, path::mutations(sp), vec![Ident::new(mut_id.into_symbol(), sp)])), Ident::new(*sym::id, sp)),
            );
            ast::mk::arm(sp, pat_some_subst, Some(guard), Some(subst))
        })
        .collect::<ThinVec<_>>();

    // NOTE: Before we evaluate any subsitution arms, we must check if the test thread is active, and
    //       if not, cancel the test thread from within to ensure that it does not start executing
    //       the code of other mutations, leading to undefined behavior.
    //       See `tests/ui/evaluation/cancel_timed_out_test_if_reenters_subst`.
    // _ if !$test_thread_active_active_expr => $test_thread_cancel_expr,
    let test_thread_active_guard_expr = ast::mk::expr_not(sp, cancellation::mk_is_test_thread_active_expr(sp));
    arms.insert(0, ast::mk::arm(sp, ast::mk::pat_wild(sp), Some(test_thread_active_guard_expr), Some(cancellation::mk_test_thread_cancel_expr(sp))));

    // _ => $default
    arms.push(ast::mk::arm(sp, ast::mk::pat_wild(sp), None, match default {
        Some(expr) => Some(expr),
        None => Some(ast::mk::expr_noop(sp)),
    }));

    // unsafe { crate::mutest_generated::ACTIVE_MUTANT_HANDLE.subst_at_unchecked($subst_loc_idx) }
    let subst_lookup_expr = ast::mk::expr_block(ast::mk::block_unsafe(sp, ast::UnsafeSource::CompilerGenerated, thin_vec![
        ast::mk::stmt_expr(ast::mk::expr_method_call_path_ident(sp, path::ACTIVE_MUTANT_HANDLE(sp), Ident::new(*sym::subst_at_unchecked, sp), thin_vec![
            ast::mk::expr_lit(sp, ast::token::LitKind::Integer, Symbol::intern(&subst_loc_idx.to_string()), None),
        ])),
    ]));

    // match unsafe { crate::mutest_generated::ACTIVE_MUTANT_HANDLE.subst_at_unchecked($subst_loc_idx) } { ... }
    ast::mk::expr_paren(sp, ast::mk::expr_match(sp, subst_lookup_expr, arms))
}

pub fn expand_subst_match_expr(sp: Span, subst_loc: SubstLoc, subst_loc_idx: usize, original: Option<P<ast::Expr>>, substs: Vec<(MutId, &Subst)>) -> P<ast::Expr> {
    let subst_exprs = substs.into_iter()
        .map(|(mut_id, subst)| {
            let subst_expr = match subst {
                Subst::AstExpr(expr) => P(expr.clone()),
                Subst::AstStmt(stmt) => ast::mk::expr_block(ast::mk::block(sp, thin_vec![stmt.clone()])),
                Subst::AstLocal(..) => panic!("invalid substitution: local substitutions cannot be made in expression positions"),
            };

            (mut_id, subst_expr)
        })
        .collect::<Vec<_>>();

    mk_subst_match_expr(sp, subst_loc, subst_loc_idx, original, subst_exprs)
}

pub fn expand_subst_match_stmt(sp: Span, subst_loc: SubstLoc, subst_loc_idx: usize, original: Option<ast::Stmt>, substs: Vec<(MutId, &Subst)>) -> Vec<ast::Stmt> {
    let mut binding_substs: Vec<(MutId, (Ident, ast::Mutability, Option<P<ast::Ty>>, P<ast::Expr>, Option<P<ast::Expr>>))> = vec![];
    let mut non_binding_substs: Vec<(MutId, &Subst)> = vec![];

    for (mut_id, subst) in substs {
        match subst {
            Subst::AstLocal(ident, mutbl, ty, expr, default_expr) => {
                binding_substs.push((mut_id, (*ident, *mutbl, ty.clone(), expr.clone(), default_expr.clone())));
            }
            _ => non_binding_substs.push((mut_id, subst)),
        }
    }

    let mut stmts = Vec::with_capacity(binding_substs.len() + !non_binding_substs.is_empty() as usize);

    for (mut_id, (ident, mutbl, ty, expr, default_expr)) in binding_substs {
        // By default, a shadowing substitution is assumed, which can be reduced to identity by
        // assigning the value of the previous binding with the same identifier to the new binding
        // (and copying all of the properties of the original binding): `let $ident = $ident`.
        let default_expr = default_expr.unwrap_or_else(|| ast::mk::expr_ident(sp, ident));
        let subst_match_expr = mk_subst_match_expr(sp, subst_loc, subst_loc_idx, Some(default_expr), vec![(mut_id, expr)]);

        let mutbl = matches!(mutbl, ast::Mutability::Mut);
        stmts.push(ast::mk::stmt_let(sp, mutbl, ident, ty, subst_match_expr));
    }

    if !non_binding_substs.is_empty() {
        let original_expr = original.map(|v| ast::mk::expr_block(ast::mk::block(sp, thin_vec![v])));
        stmts.push(ast::mk::stmt_expr(expand_subst_match_expr(sp, subst_loc, subst_loc_idx, original_expr, non_binding_substs)));
    }

    stmts
}

struct SubstWriter<'tcx, 'op> {
    sess: &'tcx Session,
    substitutions: FxHashMap<SubstLoc, Vec<(MutId, &'op Subst)>>,
    def_site: Span,
    indexed_subst_locs: Vec<SubstLoc>,
}

impl<'tcx, 'op> ast::mut_visit::MutVisitor for SubstWriter<'tcx, 'op> {
    fn visit_crate(&mut self, krate: &mut ast::Crate) {
        let g = &self.sess.psess.attr_id_generator;

        // #[allow(unused_parens)]
        let allow_unused_parens_attr = ast::mk::attr_inner(g, self.def_site,
            Ident::new(sym::allow, self.def_site),
            ast::mk::attr_args_delimited(self.def_site, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                ast::mk::tt_token_joint(self.def_site, ast::TokenKind::Ident(*sym::unused_parens, ast::token::IdentIsRaw::No)),
            ])),
        );

        krate.attrs.push(allow_unused_parens_attr);

        ast::mut_visit::walk_crate(self, krate);
    }

    fn visit_block(&mut self, block: &mut ast::Block) {
        ast::mut_visit::walk_block(self, block);

        let mut i = 0;
        while i < block.stmts.len() {
            let ast::Stmt { id: stmt_id, span: stmt_span, .. } = block.stmts[i];

            let insert_before_loc = SubstLoc::InsertBefore(stmt_id, stmt_span);
            if let Some(insertions_before) = self.substitutions.remove(&insert_before_loc) {
                let subst_loc_idx = self.indexed_subst_locs.len();
                self.indexed_subst_locs.push(insert_before_loc);

                let replacement_stmts = expand_subst_match_stmt(self.def_site, insert_before_loc, subst_loc_idx, None, insertions_before);
                let replacement_stmts_count = replacement_stmts.len();

                block.stmts.splice(i..i, replacement_stmts);

                i += replacement_stmts_count;
            }

            let replacement_loc = SubstLoc::Replace(stmt_id, stmt_span);
            if let Some(replacements) = self.substitutions.remove(&replacement_loc) {
                let subst_loc_idx = self.indexed_subst_locs.len();
                self.indexed_subst_locs.push(replacement_loc);

                let replacement_stmts = expand_subst_match_stmt(self.def_site, insert_before_loc, subst_loc_idx, None, replacements);
                let replacement_stmts_count = replacement_stmts.len();

                block.stmts.splice(i..i, replacement_stmts);

                i += replacement_stmts_count - 1;
            }

            let insert_after_loc = SubstLoc::InsertAfter(stmt_id, stmt_span);
            if let Some(insertions_after) = self.substitutions.remove(&insert_after_loc) {
                let subst_loc_idx = self.indexed_subst_locs.len();
                self.indexed_subst_locs.push(insert_after_loc);

                let replacement_stmts = expand_subst_match_stmt(self.def_site, insert_after_loc, subst_loc_idx, None, insertions_after);
                let replacement_stmts_count = replacement_stmts.len();
                i += replacement_stmts_count;

                block.stmts.splice(i..i, replacement_stmts);
            }

            i += 1;
        }
    }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        match &mut expr.kind {
            // The AST printer does not print the field name, only the expression, when using shorthand syntax. This
            // happens even if the field's expression is not an ident matching the field's name, resulting in malformed
            // code. To counter this, we force the printer to not use shorthand syntax when the field's expression is
            // substituted.
            ast::ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    assert!(!self.substitutions.contains_key(&SubstLoc::Replace(field.id, field.span)), "field expression may not be mutated directly");
                    if self.substitutions.contains_key(&SubstLoc::Replace(field.expr.id, field.expr.span)) {
                        field.is_shorthand = false;
                    }
                }
            }
            _ => {}
        }

        ast::mut_visit::walk_expr(self, expr);

        if let Some(_insertions_before) = self.substitutions.remove(&SubstLoc::InsertBefore(expr.id, expr.span)) {
            panic!("invalid substitution: substitutions cannot be inserted before expressions");
        }

        let replacement_loc = SubstLoc::Replace(expr.id, expr.span);
        if let Some(replacements) = self.substitutions.remove(&replacement_loc) {
            let subst_loc_idx = self.indexed_subst_locs.len();
            self.indexed_subst_locs.push(replacement_loc);

            *expr = expand_subst_match_expr(expr.span, replacement_loc, subst_loc_idx, Some(expr.clone()), replacements);
        }

        if let Some(_insertions_after) = self.substitutions.remove(&SubstLoc::InsertAfter(expr.id, expr.span)) {
            panic!("invalid substitution: substitutions cannot be inserted after expressions");
        }
    }
}

pub fn write_substitutions<'tcx>(tcx: TyCtxt<'tcx>, mutants: &[Mutant], krate: &mut ast::Crate) -> Vec<SubstLoc> {
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

    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let n_subst_locs = substitutions.len();

    // TODO: Warn if any substitutions have not been written to the AST. (e.g. they were defined for nodes which are not substitutable)
    let mut subst_writer = SubstWriter {
        sess: tcx.sess,
        substitutions,
        def_site,
        indexed_subst_locs: Vec::with_capacity(n_subst_locs),
    };
    subst_writer.visit_crate(krate);

    subst_writer.indexed_subst_locs
}

struct SyntaxAmbiguityResolver<'tcx> {
    _sess: &'tcx Session,
    _def_site: Span,
}

impl<'tcx> ast::mut_visit::MutVisitor for SyntaxAmbiguityResolver<'tcx> {
    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        ast::mut_visit::walk_expr(self, expr);

        match &expr.kind {
            // Expressions compared with a cast expression may be misinterpreted as type arguments for the type in the
            // cast expression. To avoid this, we simply parenthesize every cast expression.
            ast::ExprKind::Cast(_, _) => {
                *expr = ast::mk::expr_paren(expr.span, expr.clone())
            }
            _ => {}
        }
    }
}

pub fn resolve_syntax_ambiguities<'tcx>(tcx: TyCtxt<'tcx>, krate: &mut ast::Crate) {
    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut syntax_amiguity_resolver = SyntaxAmbiguityResolver { _sess: tcx.sess, _def_site: def_site };
    syntax_amiguity_resolver.visit_crate(krate);
}
