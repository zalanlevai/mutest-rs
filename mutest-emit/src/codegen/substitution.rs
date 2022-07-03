use rustc_expand::base::ResolverExpand;
use rustc_hash::FxHashMap;
use rustc_resolve::Resolver;

use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::mut_visit::MutVisitor;
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
            SubstLoc::InsertBefore(node_id) => format!("subst_bef_{}", node_id),
            SubstLoc::InsertAfter(node_id) => format!("subst_aft_{}", node_id),
            SubstLoc::Replace(node_id) => format!("subst_rep_{}", node_id),
        }
    }

    pub fn into_symbol(&self) -> Symbol {
        Symbol::intern(&self.into_symbol_name())
    }
}

fn mk_subst_match_expr(sp: Span, subst_loc: SubstLoc, default: Option<P<ast::Expr>>, substs: Vec<(MutId, P<ast::Expr>)>) -> P<ast::Expr> {
    let mut arms = substs.into_iter()
        .map(|(mut_id, subst)| {
            // Some(subst) if subst.mutation.id == crate::mutest_generated::mutations::$mut_id.id => $subst,
            let subst_ident = Ident::new(Symbol::intern("subst"), sp);
            let pat_some_subst = ast::mk::pat_tuple_struct(sp, path::Some(sp), vec![ast::mk::pat_ident(sp, subst_ident)]);
            let guard = ast::mk::expr_binary(sp, ast::BinOpKind::Eq,
                ast::mk::expr_field_deep(sp, ast::mk::expr_ident(sp, subst_ident), vec![Ident::new(*sym::mutation, sp), Ident::new(*sym::id, sp)]),
                ast::mk::expr_field(sp, ast::mk::expr_path(ast::mk::pathx(sp, path::mutations(sp), vec![Ident::new(mut_id.into_symbol(), sp)])), Ident::new(*sym::id, sp)),
            );
            ast::mk::arm(sp, pat_some_subst, Some(guard), subst)
        })
        .collect::<Vec<_>>();

    // _ => $default
    arms.push(ast::mk::arm(sp, ast::mk::pat_wild(sp), None, match default {
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
    ast::mk::expr_paren(sp, ast::mk::expr_match(sp, subst_lookup_expr, arms))
}

pub fn expand_subst_match_expr(sp: Span, subst_loc: SubstLoc, original: Option<P<ast::Expr>>, substs: Vec<(MutId, &Subst)>) -> P<ast::Expr> {
    let subst_exprs = substs.into_iter()
        .map(|(mut_id, subst)| {
            let subst_expr = match subst {
                Subst::AstExpr(expr) => P(expr.clone()),
                Subst::AstStmt(stmt) => ast::mk::expr_block(ast::mk::block(sp, vec![stmt.clone()])),
                Subst::AstLocal(..) => panic!("invalid substitution: local substitutions cannot be made in expression positions"),
            };

            (mut_id, subst_expr)
        })
        .collect::<Vec<_>>();

    mk_subst_match_expr(sp, subst_loc, original, subst_exprs)
}

pub fn expand_subst_match_stmt(sp: Span, subst_loc: SubstLoc, original: Option<ast::Stmt>, substs: Vec<(MutId, &Subst)>) -> Vec<ast::Stmt> {
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
        let subst_match_expr = mk_subst_match_expr(sp, subst_loc, Some(default_expr), vec![(mut_id, expr)]);

        let mutbl = matches!(mutbl, ast::Mutability::Mut);
        stmts.push(ast::mk::stmt_let(sp, mutbl, ident, ty, subst_match_expr));
    }

    if !non_binding_substs.is_empty() {
        let original_expr = original.map(|v| ast::mk::expr_block(ast::mk::block(sp, vec![v])));
        stmts.push(ast::mk::stmt_expr(expand_subst_match_expr(sp, subst_loc, original_expr, non_binding_substs)));
    }

    stmts
}

struct SubstWriter<'op> {
    pub substitutions: FxHashMap<SubstLoc, Vec<(MutId, &'op Subst)>>,
    pub def_site: Span,
}

impl<'op> ast::mut_visit::MutVisitor for SubstWriter<'op> {
    fn visit_crate(&mut self, krate: &mut ast::Crate) {
        // #[allow(unused_parens)]
        let allow_unused_parens_attr = ast::attr::mk_attr_inner(ast::attr::mk_list_item(
            Ident::new(sym::allow, self.def_site),
            vec![ast::attr::mk_nested_word_item(Ident::new(*sym::unused_parens, self.def_site))],
        ));

        krate.attrs.push(allow_unused_parens_attr);

        ast::mut_visit::noop_visit_crate(krate, self);
    }

    fn visit_block(&mut self, block: &mut P<ast::Block>) {
        ast::mut_visit::noop_visit_block(block, self);

        let mut i = 0;
        while i < block.stmts.len() {
            let stmt_id = block.stmts[i].id;

            let insert_before_loc = SubstLoc::InsertBefore(stmt_id);
            if let Some(insertions_before) = self.substitutions.remove(&insert_before_loc) {
                let replacement_stmts = expand_subst_match_stmt(self.def_site, insert_before_loc, None, insertions_before);
                let replacement_stmts_count = replacement_stmts.len();

                block.stmts.splice(i..i, replacement_stmts);

                i += replacement_stmts_count;
            }

            let replacement_loc = SubstLoc::Replace(stmt_id);
            if let Some(replacements) = self.substitutions.remove(&replacement_loc) {
                let replacement_stmts = expand_subst_match_stmt(self.def_site, insert_before_loc, None, replacements);
                let replacement_stmts_count = replacement_stmts.len();

                block.stmts.splice(i..i, replacement_stmts);

                i += replacement_stmts_count - 1;
            }

            let insert_after_loc = SubstLoc::InsertAfter(stmt_id);
            if let Some(insertions_after) = self.substitutions.remove(&insert_after_loc) {
                let replacement_stmts = expand_subst_match_stmt(self.def_site, insert_after_loc, None, insertions_after);
                let replacement_stmts_count = replacement_stmts.len();
                i += replacement_stmts_count;

                block.stmts.splice(i..i, replacement_stmts);
            }

            i += 1;
        }
    }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        ast::mut_visit::noop_visit_expr(expr, self);

        match &mut expr.kind {
            // The AST printer does not print the field name, only the expression, when using shorthand syntax. This
            // happens even if the field's expression is not an ident matching the field's name, resulting in malformed
            // code. To counter this, we force the printer to not use shorthand syntax when the field's expression is
            // substituted.
            ast::ExprKind::Struct(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if self.substitutions.contains_key(&SubstLoc::Replace(field.id)) {
                        field.is_shorthand = false;
                    }
                }
            }
            _ => {}
        }

        let expr_id = expr.id;

        if let Some(_insertions_before) = self.substitutions.remove(&SubstLoc::InsertBefore(expr_id)) {
            panic!("invalid substitution: substitutions cannot be inserted before expressions");
        }

        let replacement_loc = SubstLoc::Replace(expr_id);
        if let Some(replacements) = self.substitutions.remove(&replacement_loc) {
            *expr = expand_subst_match_expr(expr.span, replacement_loc, Some(expr.clone()), replacements);
        }

        if let Some(_insertions_after) = self.substitutions.remove(&SubstLoc::InsertAfter(expr_id)) {
            panic!("invalid substitution: substitutions cannot be inserted after expressions");
        }
    }
}

pub fn write_substitutions(resolver: &mut Resolver, mutants: &Vec<Mutant>, krate: &mut ast::Crate) {
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

    let expn_id = resolver.expansion_for_ast_pass(
        DUMMY_SP,
        AstPass::TestHarness,
        &[sym::rustc_attrs],
        None,
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    // TODO: Warn if any substitutions have not been written to the AST. (e.g. they were defined for nodes which are not substitutable)
    let mut subst_writer = SubstWriter { substitutions, def_site };
    subst_writer.visit_crate(krate);
}
