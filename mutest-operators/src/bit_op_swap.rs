use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use smallvec::{SmallVec, smallvec};

macro define_bit_op_swap_mutation(
    $(#[$meta:meta])*
    $vis:vis $operator:ident, $mutation:ident {
        $($bin_op_from:pat => $bin_op_to:expr),+ $(,)?
    }
) {
    $vis struct $mutation {
        pub is_assignment: bool,
        pub original_bin_op: ast::BinOpKind,
        pub replacement_bin_op: ast::BinOpKind,
    }

    impl Mutation for $mutation {
        fn display_name(&self) -> String {
            format!("swap bitwise {expr_kind} `{original_bin_op}` for `{replacement_bin_op}`",
                expr_kind = match self.is_assignment {
                    true => "assignment operator",
                    false => "operator",
                },
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

            let bin_op = match &expr.ast.kind {
                ast::ExprKind::Binary(bin_op, _, _) => bin_op,
                ast::ExprKind::AssignOp(bin_op, _, _) => bin_op,
                _ => { return None; }
            };

            let mapped_bin_op = match bin_op.node {
                $($bin_op_from => $bin_op_to,)+
                _ => { return None; },
            };

            let mapped_bin_expr = match &expr.ast.kind {
                ast::ExprKind::Binary(_, lhs, rhs) => ast::mk::expr_binary(def, mapped_bin_op, lhs.clone(), rhs.clone()),
                ast::ExprKind::AssignOp(_, lhs, rhs) => ast::mk::expr_assign_op(def, mapped_bin_op, lhs.clone(), rhs.clone()),
                _ => unreachable!(),
            };

            let mutation = Self::Mutation {
                is_assignment: matches!(&expr.ast.kind, ast::ExprKind::AssignOp(_, _, _)),
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

define_bit_op_swap_mutation! {
    /// Swap bitwise OR for bitwise XOR and vice versa.
    pub BitOpOrXorSwap, BitOpOrXorSwapMutation {
        ast::BinOpKind::BitOr => ast::BinOpKind::BitXor,
        ast::BinOpKind::BitXor => ast::BinOpKind::BitOr,
    }
}

define_bit_op_swap_mutation! {
    /// Swap bitwise OR for bitwise AND and vice versa.
    pub BitOpOrAndSwap, BitOpOrAndSwapMutation {
        ast::BinOpKind::BitOr => ast::BinOpKind::BitAnd,
        ast::BinOpKind::BitAnd => ast::BinOpKind::BitOr,
    }
}

define_bit_op_swap_mutation! {
    /// Swap bitwise XOR for bitwise AND and vice versa.
    pub BitOpXorAndSwap, BitOpXorAndSwapMutation {
        ast::BinOpKind::BitXor => ast::BinOpKind::BitAnd,
        ast::BinOpKind::BitAnd => ast::BinOpKind::BitXor,
    }
}

define_bit_op_swap_mutation! {
    /// Swap the direction of bitwise shift operators.
    pub BitOpShiftDirSwap, BitOpShiftDirSwapMutation {
        ast::BinOpKind::Shl => ast::BinOpKind::Shr,
        ast::BinOpKind::Shr => ast::BinOpKind::Shl,
    }
}
