use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, Symbol, sym};
use mutest_emit::smallvec::smallvec;
use mutest_emit::thin_vec::thin_vec;

pub const BOOL_EXPR_NEGATE: &str = "bool_expr_negate";

pub struct BoolExprNegateMutation {
    pub was_negated: bool,
}

impl Mutation for BoolExprNegateMutation {
    fn op_name(&self) -> &str { BOOL_EXPR_NEGATE }

    fn display_name(&self) -> String {
        format!("{operation} boolean expression",
            operation = match self.was_negated {
                false => "negate",
                true => "remove negation from",
            },
        )
    }
}

/// Negate boolean expressions.
pub struct BoolExprNegate;

impl<'a> Operator<'a> for BoolExprNegate {
    type Mutation = BoolExprNegateMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx, crate_res: _, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _f) = location else { return Mutations::none(); };

        if let ast::ExprKind::Let(_, _, _, _) = expr.kind { return Mutations::none(); };

        let Some(body_hir) = f_hir.body else { return Mutations::none(); };
        let typeck = tcx.typeck_body(body_hir.id());

        let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };
        let expr_ty = typeck.expr_ty(expr_hir);
        if expr_ty != tcx.types.bool { return Mutations::none(); }

        let unambiguous_base_expr = match &expr.kind {
            // NOTE: Calls to generic functions with generic return types (e.g. `Default::default`)
            //       may not be inferrable once the call is wrapped in a negation, see
            //       `tests/ui/mutation/ops/bool_expr_negate/rustc_res/cannot_infer_negated_generic_call`.
            //       To avoid this, we ascribe the `bool` type to the return value of the call expression
            //       by creating a let binding in a block expression with the binding as its value.
            ast::ExprKind::Call(_, _) | ast::ExprKind::MethodCall(_) => {
                let expr_ty_ast = ast::mk::ty_ident(def, None, Ident::new(sym::bool, def));

                // { let v: bool = $expr; v }
                let v = Ident::new(Symbol::intern("v"), def);
                ast::mk::expr_block(ast::mk::block(def, thin_vec![
                    ast::mk::stmt_let(def, false, v, Some(expr_ty_ast), Box::new(expr.clone())),
                    ast::mk::stmt_expr(ast::mk::expr_ident(def, v)),
                ]))
            }
            _ => Box::new(expr.clone()),
        };

        let negated_expr = ast::mk::expr_unary(def, ast::UnOp::Not, unambiguous_base_expr);

        let mutation = Self::Mutation {
            was_negated: matches!(&expr.kind, ast::ExprKind::Unary(ast::UnOp::Not, _)),
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id, expr.span),
                Subst::AstExpr(*negated_expr),
            ),
        ])
    }
}
