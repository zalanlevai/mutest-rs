use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use smallvec::{SmallVec, smallvec};

macro define_math_op_swap_mutation(
    $(#[$meta:meta])*
    $vis:vis $operator:ident, $mutation:ident {
        $($bin_op_from:pat => $bin_op_to:expr),+ $(,)?
    }
) {
    $vis struct $mutation {
        pub original_bin_op: ast::BinOpKind,
        pub replacement_bin_op: ast::BinOpKind,
    }

    impl Mutation for $mutation {
        fn display_name(&self) -> String {
            format!("swap math operator `{original_bin_op}` for `{replacement_bin_op}`",
                original_bin_op = self.original_bin_op.to_string(),
                replacement_bin_op = self.replacement_bin_op.to_string(),
            )
        }
    }

    $(#[$meta])*
    $vis struct $operator;

    impl<'a> Operator<'a> for $operator {
        type Mutation = $mutation;

        fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
            let MutCtxt { tcx: _, resolver: _, def_site: def, ref location } = *mcx;

            let MutLoc::FnBodyExpr(expr, _) = location else { return None; };

            let ast::ExprKind::Binary(bin_op, lhs, rhs) = &expr.ast.kind else { return None; };

            let mapped_bin_op = match bin_op.node {
                $($bin_op_from => $bin_op_to,)+
                _ => { return None; },
            };
            let mapped_bin_expr = ast::mk::expr_binary(def, mapped_bin_op, lhs.clone(), rhs.clone());

            let mutation = Self::Mutation {
                original_bin_op: bin_op.node,
                replacement_bin_op: mapped_bin_op,
            };

            Some((mutation, smallvec![
                SubstDef::new(
                    SubstLoc::Replace(expr.ast.id),
                    Subst::AstExpr(mapped_bin_expr.into_inner()),
                ),
            ]))
        }
    }
}

define_math_op_swap_mutation! {
    /// Swap addition for subtraction and vice versa.
    pub MathOpAddSubSwap, MathOpAddSubSwapMutation {
        ast::BinOpKind::Add => ast::BinOpKind::Sub,
        ast::BinOpKind::Sub => ast::BinOpKind::Add,
    }
}

define_math_op_swap_mutation! {
    /// Swap addition for multiplication and vice versa.
    pub MathOpAddMulSwap, MathOpAddMulSwapMutation {
        ast::BinOpKind::Add => ast::BinOpKind::Mul,
        ast::BinOpKind::Mul => ast::BinOpKind::Add,
    }
}

define_math_op_swap_mutation! {
    /// Swap multiplication for division and vice versa.
    pub MathOpMulDivSwap, MathOpMulDivSwapMutation {
        ast::BinOpKind::Mul => ast::BinOpKind::Div,
        ast::BinOpKind::Div => ast::BinOpKind::Mul,
    }
}

define_math_op_swap_mutation! {
    /// Swap division for modulus and vice versa.
    pub MathOpDivRemSwap, MathOpDivRemSwapMutation {
        ast::BinOpKind::Div => ast::BinOpKind::Rem,
        ast::BinOpKind::Rem => ast::BinOpKind::Div,
    }
}
