pub mod visit {
    use std::iter;

    use rustc_ast as ast;
    use rustc_hir as hir;
    use rustc_hir::intravisit::Map;
    use rustc_hir::intravisit::nested_filter::{self, NestedFilter};
    use rustc_span::Span;

    pub trait AstHirVisitor<'ast, 'hir>: Sized {
        type Map: hir::intravisit::Map<'hir> = <Self::NestedFilter as NestedFilter<'hir>>::Map;
        type NestedFilter: NestedFilter<'hir> = nested_filter::None;

        fn nested_visit_map(&mut self) -> Self::Map {
            panic!(
                "nested_visit_map must be implemented or consider using \
                `type NestedFilter = nested_filter::None` (the default)"
            );
        }

        fn nested_item(&mut self, id: hir::ItemId) -> Option<&'hir hir::Item<'hir>> {
            Self::NestedFilter::INTER.then(|| self.nested_visit_map().item(id))
        }

        fn nested_trait_item(&mut self, id: hir::TraitItemId) -> Option<&'hir hir::TraitItem<'hir>> {
            Self::NestedFilter::INTER.then(|| self.nested_visit_map().trait_item(id))
        }

        fn nested_impl_item(&mut self, id: hir::ImplItemId) -> Option<&'hir hir::ImplItem<'hir>> {
            Self::NestedFilter::INTER.then(|| self.nested_visit_map().impl_item(id))
        }

        fn nested_foreign_item(&mut self, id: hir::ForeignItemId) -> Option<&'hir hir::ForeignItem<'hir>> {
            Self::NestedFilter::INTER.then(|| self.nested_visit_map().foreign_item(id))
        }

        fn nested_body(&mut self, id: hir::BodyId) -> Option<&'hir hir::Body<'hir>> {
            Self::NestedFilter::INTRA.then(|| self.nested_visit_map().body(id))
        }

        fn visit_fn(&mut self, kind_ast: ast::visit::FnKind<'ast>, span_ast: Span, id_ast: ast::NodeId, kind_hir: hir::intravisit::FnKind<'hir>, decl_hir: &'hir hir::FnDecl<'hir>, body_hir: hir::BodyId, span_hir: Span, id_hir: hir::HirId) {
            walk_fn(self, kind_ast, span_ast, id_ast, kind_hir, decl_hir, body_hir, span_hir, id_hir);
        }

        fn visit_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>) {
            walk_param(self, param_ast, param_hir);
        }

        fn visit_block(&mut self, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
            walk_block(self, block_ast, block_hir);
        }

        fn visit_stmt(&mut self, stmt_ast: &'ast ast::Stmt, stmt_hir: &'hir hir::Stmt<'hir>) {
            walk_stmt(self, stmt_ast, stmt_hir);
        }

        fn visit_expr(&mut self, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
            walk_expr(self, expr_ast, expr_hir);
        }
    }

    pub fn walk_fn<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, kind_ast: ast::visit::FnKind<'ast>, span_ast: Span, id_ast: ast::NodeId, kind_hir: hir::intravisit::FnKind<'hir>, decl_hir: &'hir hir::FnDecl<'hir>, body_hir: hir::BodyId, span_hir: Span, id_hir: hir::HirId) {
        match (kind_ast, kind_hir) {
            | (ast::visit::FnKind::Fn(_, _, sig_ast, _, body_ast), hir::intravisit::FnKind::ItemFn(_, _, _, _))
            | (ast::visit::FnKind::Fn(_, _, sig_ast, _, body_ast), hir::intravisit::FnKind::Method(_, _, _)) => {
                if let Some(body_hir) = visitor.nested_body(body_hir) {
                    for (param_ast, param_hir) in iter::zip(&sig_ast.decl.inputs, body_hir.params) {
                        visitor.visit_param(param_ast, param_hir);
                    }

                    if let Some(body_ast) = body_ast {
                        visit_block_expr(visitor, body_ast, &body_hir.value);
                    }
                }
            }
            (ast::visit::FnKind::Closure(decl_ast, expr_ast), hir::intravisit::FnKind::Closure) => {
                if let Some(body_hir) = visitor.nested_body(body_hir) {
                    for (param_ast, param_hir) in iter::zip(&decl_ast.inputs, body_hir.params) {
                        visitor.visit_param(param_ast, param_hir);
                    }

                    visit_matching_expr(visitor, expr_ast, &body_hir.value);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn walk_param<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>) {}

    pub fn visit_block_expr<'ast, 'hir, T:AstHirVisitor<'ast, 'hir>>(visitor: &mut T, block_ast: &'ast ast::Block, expr_hir: &'hir hir::Expr<'hir>) {
        if let hir::ExprKind::Block(expr_hir_block, _) = expr_hir.kind {
            visitor.visit_block(block_ast, expr_hir_block);
        }
    }

    pub fn walk_block<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
        let block_ast_stmts = || block_ast.stmts.iter()
            .filter(|stmt| !matches!(stmt.kind, ast::StmtKind::Empty | ast::StmtKind::MacCall(_)));

        for (stmt_ast, stmt_hir) in iter::zip(block_ast_stmts(), block_hir.stmts) {
            visitor.visit_stmt(stmt_ast, stmt_hir);
        }

        if let Some(block_hir_expr) = block_hir.expr && let Some(block_ast_stmt) = block_ast_stmts().last() {
            let ast::StmtKind::Expr(block_ast_stmt_expr) = &block_ast_stmt.kind else { unreachable!() };
            visit_matching_expr(visitor, block_ast_stmt_expr, block_hir_expr);
        }
    }

    pub fn walk_stmt<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, stmt_ast: &'ast ast::Stmt, stmt_hir: &'hir hir::Stmt<'hir>) {
        match (&stmt_ast.kind, &stmt_hir.kind) {
            | (ast::StmtKind::Expr(expr_ast), hir::StmtKind::Expr(expr_hir))
            | (ast::StmtKind::Semi(expr_ast), hir::StmtKind::Semi(expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }

            (ast::StmtKind::Local(local_ast), hir::StmtKind::Local(local_hir)) => {
                match (&local_ast.kind, local_hir.init) {
                    | (ast::LocalKind::Init(expr_ast), Some(expr_hir))
                    | (ast::LocalKind::InitElse(expr_ast, _), Some(expr_hir)) => {
                        visit_matching_expr(visitor, expr_ast, expr_hir);
                    }

                    (ast::LocalKind::Decl, None) => {}
                    _ => unreachable!(),
                }
            }

            (ast::StmtKind::Item(item_ast), hir::StmtKind::Item(item_hir)) => {
                // TODO
            }

            (ast::StmtKind::Empty, _) | (ast::StmtKind::MacCall(_), _) => {}
            _ => unreachable!(),
        }
    }

    pub fn visit_matching_expr<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
        match (&expr_ast.kind, &expr_hir.kind) {
            (_, hir::ExprKind::DropTemps(expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Paren(expr_ast), _) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }

            _ => visitor.visit_expr(expr_ast, expr_hir),
        }
    }

    pub fn walk_expr<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
        match (&expr_ast.kind, &expr_hir.kind) {
            (_, hir::ExprKind::DropTemps(expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }

            (ast::ExprKind::Box(expr_ast), hir::ExprKind::Box(expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Array(exprs_ast), hir::ExprKind::Array(exprs_hir)) => {
                for (expr_ast, expr_hir) in iter::zip(exprs_ast, *exprs_hir) {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::ConstBlock(anon_const_ast), hir::ExprKind::ConstBlock(anon_const_hir)) => {
                // TODO
            }
            (ast::ExprKind::Call(expr_ast, args_ast), hir::ExprKind::Call(expr_hir, args_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                for (arg_ast, arg_hir) in iter::zip(args_ast, *args_hir) {
                    visit_matching_expr(visitor, arg_ast, arg_hir);
                }
            }
            (ast::ExprKind::MethodCall(_, exprs_ast, _), hir::ExprKind::MethodCall(_, exprs_hir, _)) => {
                for (expr_ast, expr_hir) in iter::zip(exprs_ast, *exprs_hir) {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::Tup(exprs_ast), hir::ExprKind::Tup(exprs_hir)) => {
                for (expr_ast, expr_hir) in iter::zip(exprs_ast, *exprs_hir) {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::Binary(_, left_ast, right_ast), hir::ExprKind::Binary(_, left_hir, right_hir)) => {
                visit_matching_expr(visitor, left_ast, left_hir);
                visit_matching_expr(visitor, right_ast, right_hir);
            }
            (ast::ExprKind::Unary(_, expr_ast), hir::ExprKind::Unary(_, expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Lit(_), hir::ExprKind::Lit(_)) => {
                // TODO
            }
            (ast::ExprKind::Cast(expr_ast, _), hir::ExprKind::Cast(expr_hir, _)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Type(expr_ast, _), hir::ExprKind::Type(expr_hir, _)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Let(_, expr_ast, _), hir::ExprKind::Let(let_hir)) => {
                visit_matching_expr(visitor, expr_ast, let_hir.init);
            }
            (ast::ExprKind::If(cond_ast, then_ast, els_ast), hir::ExprKind::If(cond_hir, then_hir, els_hir)) => {
                visit_matching_expr(visitor, cond_ast, cond_hir);
                visit_block_expr(visitor, then_ast, then_hir);
                if let Some(els_ast) = els_ast && let Some(els_hir) = els_hir {
                    visit_matching_expr(visitor, els_ast, els_hir);
                }
            }
            (ast::ExprKind::While(_, _, _), hir::ExprKind::Loop(_, _, hir::LoopSource::While, _)) => {
                // TODO
            }
            (ast::ExprKind::ForLoop(_, _, _, _), hir::ExprKind::Loop(_, _, hir::LoopSource::ForLoop, _)) => {
                // TODO
            }
            (ast::ExprKind::Loop(block_ast, _), hir::ExprKind::Loop(block_hir, _, hir::LoopSource::Loop, _)) => {
                visitor.visit_block(block_ast, block_hir);
            }
            (ast::ExprKind::Match(expr_ast, _), hir::ExprKind::Match(expr_hir, _, hir::MatchSource::Normal)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                // TODO: Visit match arms.
            }
            (ast::ExprKind::Closure(_, _, _, _, _, _), hir::ExprKind::Closure(_, _, _, _, _)) => {
                // TODO
            }
            (ast::ExprKind::Block(block_ast, _), hir::ExprKind::Block(block_hir, _)) => {
                visitor.visit_block(block_ast, block_hir);
            }
            (ast::ExprKind::Async(_, _, _), _) => {
                // TODO
            }
            (ast::ExprKind::Await(expr_ast), hir::ExprKind::Match(expr_hir, _, hir::MatchSource::AwaitDesugar)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::TryBlock(_), _) => {
                // TODO
            }
            (ast::ExprKind::Assign(left_ast, right_ast, _), hir::ExprKind::Assign(left_hir, right_hir, _)) => {
                visit_matching_expr(visitor, left_ast, left_hir);
                visit_matching_expr(visitor, right_ast, right_hir);
            }
            (ast::ExprKind::AssignOp(_, left_ast, right_ast), hir::ExprKind::AssignOp(_, left_hir, right_hir)) => {
                visit_matching_expr(visitor, left_ast, left_hir);
                visit_matching_expr(visitor, right_ast, right_hir);
            }
            (ast::ExprKind::Field(expr_ast, _), hir::ExprKind::Field(expr_hir, _)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Index(expr_ast, index_ast), hir::ExprKind::Index(expr_hir, index_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                visit_matching_expr(visitor, index_ast, index_hir);
            }
            (ast::ExprKind::Range(_, _, _), _) => {
                // TODO
            }
            (ast::ExprKind::Underscore, _) => {}
            (ast::ExprKind::Path(_, _), hir::ExprKind::Path(_)) => {
                // TODO
            }
            (ast::ExprKind::AddrOf(_, _, expr_ast), hir::ExprKind::AddrOf(_, _, expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Break(_, _), hir::ExprKind::Break(_, _)) => {
                // TODO
            }
            (ast::ExprKind::Continue(_), hir::ExprKind::Continue(_)) => {
                // TODO
            }
            (ast::ExprKind::Ret(expr_ast), hir::ExprKind::Ret(expr_hir)) => {
                if let Some(expr_ast) = expr_ast && let Some(expr_hir) = expr_hir {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::InlineAsm(_), hir::ExprKind::InlineAsm(_)) => {
                // TODO
            }
            (ast::ExprKind::MacCall(_), _) => {}
            (ast::ExprKind::Struct(_), hir::ExprKind::Struct(_, _, _)) => {
                // TODO
            }
            (ast::ExprKind::Repeat(expr_ast, anon_const_ast), hir::ExprKind::Repeat(expr_hir, anon_const_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                // TODO
            }
            (ast::ExprKind::Paren(expr_ast), _) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Try(expr_ast), hir::ExprKind::Match(expr_hir, _, hir::MatchSource::TryDesugar)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Yield(expr_ast), hir::ExprKind::Yield(expr_hir, _)) => {
                if let Some(expr_ast) = expr_ast {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }

            (ast::ExprKind::Err, _) | (_, hir::ExprKind::Err) => {}
            _ => unreachable!(),
        }
    }
}
