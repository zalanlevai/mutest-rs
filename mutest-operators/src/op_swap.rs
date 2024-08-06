use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::hir;
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty::{self, Ty, TyCtxt};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::sym;
use mutest_emit::smallvec::smallvec;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpKind {
    Standalone,
    Assign,
}

impl OpKind {
    pub fn desc(&self) -> &str {
        match self {
            Self::Standalone => "operator",
            Self::Assign => "assignment operator",
        }
    }
}

fn impls_matching_op<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, caller_def_id: hir::LocalDefId, lhs_ty: Ty<'tcx>, rhs_ty: Ty<'tcx>, expr_ty: Ty<'tcx>, op_trait: hir::DefId, op_kind: OpKind) -> bool {
    if !ty::impls_trait_with_env(tcx, param_env, lhs_ty, op_trait, vec![rhs_ty.into()]) { return false; }

    match op_kind {
        OpKind::Standalone => {
            ty::impl_assoc_ty(tcx, param_env, caller_def_id, lhs_ty, op_trait, vec![rhs_ty.into()], sym::Output)
                .map(|ty| ty == expr_ty).unwrap_or(false)
        }
        OpKind::Assign => true,
    }
}

macro define_op_swap_operator(
    $(#[$meta:meta])*
    $vis:vis $operator:ident, $mutation:ident as $op_name_ident:ident = $op_name:literal $([$bin_op_group:expr])? {
        $($bin_op_from:pat $(if impl $bin_op_to_trait:ident, $bin_assign_op_to_trait:ident)? => $bin_op_to:expr),+ $(,)?
    }
) {
    $vis const $op_name_ident: &str = $op_name;

    $vis struct $mutation {
        pub op_kind: OpKind,
        pub original_bin_op: ast::BinOpKind,
        pub replacement_bin_op: ast::BinOpKind,
    }

    impl Mutation for $mutation {
        fn op_name(&self) -> &str { $op_name }

        fn display_name(&self) -> String {
            format!(concat!("swap ", $($bin_op_group, " ",)? "{op_kind} `{original_bin_op}` for `{replacement_bin_op}`"),
                op_kind = self.op_kind.desc(),
                original_bin_op = self.original_bin_op.as_str(),
                replacement_bin_op = self.replacement_bin_op.as_str(),
            )
        }

        fn span_label(&self) -> String {
            format!(concat!("swap ", $($bin_op_group, " ",)? "{op_kind} for `{replacement_bin_op}`"),
                op_kind = self.op_kind.desc(),
                replacement_bin_op = self.replacement_bin_op.as_str(),
            )
        }
    }

    $(#[$meta])*
    $vis struct $operator;

    impl<'a> Operator<'a> for $operator {
        type Mutation = $mutation;

        fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
            let MutCtxt { opts: _, tcx, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

            let MutLoc::FnBodyExpr(expr, _f) = location else { return Mutations::none(); };

            let (bin_op, op_kind) = match &expr.kind {
                ast::ExprKind::Binary(bin_op, _, _) => (bin_op.node, OpKind::Standalone),
                ast::ExprKind::AssignOp(bin_op, _, _) => (bin_op.node, OpKind::Assign),
                _ => { return Mutations::none(); }
            };

            let param_env = tcx.param_env(f_hir.owner_id.def_id);

            let Some(body_hir) = f_hir.body else { return Mutations::none(); };
            let typeck = tcx.typeck_body(body_hir.id());

            let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };
            let expr_ty = typeck.expr_ty(expr_hir);
            let (lhs_ty, rhs_ty) = match expr_hir.kind {
                | hir::ExprKind::Binary(_, lhs, rhs)
                | hir::ExprKind::AssignOp(_, lhs, rhs) => {
                    (typeck.expr_ty(lhs), typeck.expr_ty(rhs))
                }
                _ => unreachable!(),
            };

            let caller_def_id = f_hir.owner_id.def_id;
            #[allow(unused_variables)]
            let expr_impls_matching_op = |op_trait| impls_matching_op(tcx, param_env, caller_def_id, lhs_ty, rhs_ty, expr_ty, op_trait, op_kind);

            let mapped_bin_op = match (bin_op, op_kind) {
                $(
                    ($bin_op_from, OpKind::Standalone) $(if expr_impls_matching_op(res::traits::$bin_op_to_trait(tcx)))? => $bin_op_to,
                    ($bin_op_from, OpKind::Assign) $(if expr_impls_matching_op(res::traits::$bin_assign_op_to_trait(tcx)))? => $bin_op_to,
                )+
                _ => { return Mutations::none(); }
            };

            let mapped_bin_expr = match &expr.kind {
                ast::ExprKind::Binary(_, lhs, rhs) => ast::mk::expr_binary(def, mapped_bin_op, lhs.clone(), rhs.clone()),
                ast::ExprKind::AssignOp(_, lhs, rhs) => ast::mk::expr_assign_op(def, mapped_bin_op, lhs.clone(), rhs.clone()),
                _ => unreachable!(),
            };

            let mutation = Self::Mutation {
                op_kind,
                original_bin_op: bin_op,
                replacement_bin_op: mapped_bin_op,
            };

            Mutations::new_one(mutation, smallvec![
                SubstDef::new(
                    SubstLoc::Replace(expr.id),
                    Subst::AstExpr(mapped_bin_expr.into_inner()),
                ),
            ])
        }
    }
}

define_op_swap_operator! {
    /// Swap addition for subtraction and vice versa.
    pub OpAddSubSwap, OpAddSubSwapMutation as MATH_OP_ADD_SUB_SWAP = "math_op_add_sub_swap" {
        ast::BinOpKind::Add if impl Sub, SubAssign => ast::BinOpKind::Sub,
        ast::BinOpKind::Sub if impl Add, AddAssign => ast::BinOpKind::Add,
    }
}

define_op_swap_operator! {
    /// Swap addition for multiplication and vice versa.
    pub OpAddMulSwap, OpAddMulSwapMutation as MATH_OP_ADD_MUL_SWAP = "math_op_add_mul_swap" {
        ast::BinOpKind::Add if impl Mul, MulAssign => ast::BinOpKind::Mul,
        ast::BinOpKind::Mul if impl Add, AddAssign => ast::BinOpKind::Add,
    }
}

define_op_swap_operator! {
    /// Swap multiplication for division and vice versa.
    pub OpMulDivSwap, OpMulDivSwapMutation as MATH_OP_MUL_DIV_SWAP = "math_op_mul_div_swap" {
        ast::BinOpKind::Mul if impl Div, DivAssign => ast::BinOpKind::Div,
        ast::BinOpKind::Div if impl Mul, MulAssign => ast::BinOpKind::Mul,
    }
}

define_op_swap_operator! {
    /// Swap division for modulus and vice versa.
    pub OpDivRemSwap, OpDivRemSwapMutation as MATH_OP_DIV_REM_SWAP = "math_op_div_rem_swap" {
        ast::BinOpKind::Div if impl Rem, RemAssign => ast::BinOpKind::Rem,
        ast::BinOpKind::Rem if impl Div, DivAssign => ast::BinOpKind::Div,
    }
}

define_op_swap_operator! {
    /// Swap bitwise OR for bitwise XOR and vice versa.
    pub BitOpOrXorSwap, BitOpOrXorSwapMutation as BIT_OP_OR_XOR_SWAP = "bit_op_or_xor_swap" ["bitwise"] {
        ast::BinOpKind::BitOr if impl BitXor, BitXorAssign => ast::BinOpKind::BitXor,
        ast::BinOpKind::BitXor if impl BitOr, BitOrAssign => ast::BinOpKind::BitOr,
    }
}

define_op_swap_operator! {
    /// Swap bitwise OR for bitwise AND and vice versa.
    pub BitOpOrAndSwap, BitOpOrAndSwapMutation as BIT_OP_OR_AND_SWAP = "bit_op_or_and_swap" ["bitwise"] {
        ast::BinOpKind::BitOr if impl BitAnd, BitAndAssign => ast::BinOpKind::BitAnd,
        ast::BinOpKind::BitAnd if impl BitOr, BitOrAssign => ast::BinOpKind::BitOr,
    }
}

define_op_swap_operator! {
    /// Swap bitwise XOR for bitwise AND and vice versa.
    pub BitOpXorAndSwap, BitOpXorAndSwapMutation as BIT_OP_XOR_AND_SWAP = "bit_op_xor_and_swap" ["bitwise"] {
        ast::BinOpKind::BitXor if impl BitAnd, BitAndAssign => ast::BinOpKind::BitAnd,
        ast::BinOpKind::BitAnd if impl BitXor, BitXorAssign => ast::BinOpKind::BitXor,
    }
}

define_op_swap_operator! {
    /// Swap the direction of bitwise shift operators.
    pub BitOpShiftDirSwap, BitOpShiftDirSwapMutation as BIT_OP_SHIFT_DIR_SWAP = "bit_op_shift_dir_swap" ["bitwise"] {
        ast::BinOpKind::Shl if impl Shr, ShrAssign => ast::BinOpKind::Shr,
        ast::BinOpKind::Shr if impl Shl, ShlAssign => ast::BinOpKind::Shl,
    }
}

define_op_swap_operator! {
    /// Swap logical && for logical || and vice versa.
    pub LogicalOpAndOrSwap, LogicalOpAndOrSwapMutation as LOGICAL_OP_AND_OR_SWAP = "logical_op_and_or_swap" ["logical"] {
        ast::BinOpKind::And => ast::BinOpKind::Or,
        ast::BinOpKind::Or => ast::BinOpKind::And,
    }
}
