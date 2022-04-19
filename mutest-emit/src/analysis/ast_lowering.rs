use std::collections::VecDeque;

use crate::analysis::hir;
use crate::analysis::ty::TyCtxt;
use crate::analysis::res;
use crate::codegen::ast;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::symbols::{Ident, Span};

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

    pub fn walk_fn<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, kind_ast: ast::visit::FnKind<'ast>, _span_ast: Span, _id_ast: ast::NodeId, kind_hir: hir::intravisit::FnKind<'hir>, _decl_hir: &'hir hir::FnDecl<'hir>, body_hir: hir::BodyId, _span_hir: Span, _id_hir: hir::HirId) {
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

    pub fn walk_param<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(_visitor: &mut T, _param_ast: &'ast ast::Param, _param_hir: &'hir hir::Param<'hir>) {}

    pub fn visit_block_expr<'ast, 'hir, T:AstHirVisitor<'ast, 'hir>>(visitor: &mut T, block_ast: &'ast ast::Block, expr_hir: &'hir hir::Expr<'hir>) {
        if let hir::ExprKind::Block(expr_hir_block, _) = expr_hir.kind {
            visitor.visit_block(block_ast, expr_hir_block);
        }
    }

    pub fn walk_block<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
        let block_ast_stmts = block_ast.stmts.iter()
            .filter(|stmt| !matches!(stmt.kind, ast::StmtKind::Empty | ast::StmtKind::MacCall(_)));

        for (stmt_ast, stmt_hir) in iter::zip(block_ast_stmts.clone(), block_hir.stmts) {
            visitor.visit_stmt(stmt_ast, stmt_hir);
        }

        if let Some(block_hir_expr) = block_hir.expr && let Some(block_ast_stmt) = block_ast_stmts.last() {
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

            (ast::StmtKind::Item(_item_ast), hir::StmtKind::Item(_item_hir)) => {
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
            (ast::ExprKind::Paren(expr_ast), _) => {
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
            (ast::ExprKind::ConstBlock(_anon_const_ast), hir::ExprKind::ConstBlock(_anon_const_hir)) => {
                // TODO: Visit anonymous const
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
            (ast::ExprKind::While(expr_ast, block_ast, _label_ast), hir::ExprKind::Loop(block_hir, _label_hir, hir::LoopSource::While, _)) => {
                // TODO: Visit label
                if let Some(block_expr_hir) = block_hir.expr
                    && let hir::ExprKind::If(cond_hir, then_hir, _) = block_expr_hir.kind
                {
                    visit_matching_expr(visitor, expr_ast, cond_hir);
                    if let hir::ExprKind::Block(block_hir, _) = then_hir.kind {
                        visitor.visit_block(block_ast, block_hir);
                    }
                }
            }
            (ast::ExprKind::ForLoop(_pat_ast, cond_ast, block_ast, _label_ast), hir::ExprKind::Match(expr_hir, [arm_hir], hir::MatchSource::ForLoopDesugar)) => {
                if let hir::ExprKind::Loop(block_hir, _label_hir, hir::LoopSource::ForLoop, _) = arm_hir.body.kind {
                    // TODO: Visit label
                    if let [block_stmt_hir] = block_hir.stmts
                        && let hir::StmtKind::Expr(block_expr_hir) = block_stmt_hir.kind
                        && let hir::ExprKind::Match(_, [_, some_arm_hir], hir::MatchSource::ForLoopDesugar) = block_expr_hir.kind
                    {
                        if let hir::PatKind::TupleStruct(_, [_pat_hir], _) = some_arm_hir.pat.kind {
                            // TODO: Visit pattern
                        }
                        if let hir::ExprKind::Call(_, [cond_hir]) = expr_hir.kind {
                            visit_matching_expr(visitor, cond_ast, cond_hir);
                        }
                        visit_block_expr(visitor, block_ast, some_arm_hir.body);
                    }
                }
            }
            (ast::ExprKind::Loop(block_ast, _), hir::ExprKind::Loop(block_hir, _, hir::LoopSource::Loop, _)) => {
                visitor.visit_block(block_ast, block_hir);
            }
            (ast::ExprKind::Match(expr_ast, arms_ast), hir::ExprKind::Match(expr_hir, arms_hir, hir::MatchSource::Normal)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                for (arm_ast, arm_hir) in iter::zip(arms_ast, *arms_hir) {
                    // TODO: Visit pattern
                    visit_matching_expr(visitor, &arm_ast.body, arm_hir.body);
                }
            }
            (ast::ExprKind::Closure(_, _, _, decl_ast, expr_ast, _), hir::ExprKind::Closure(_, _, body_hir, _, _)) => {
                if let Some(body_hir) = visitor.nested_body(*body_hir) {
                    for (param_ast, param_hir) in iter::zip(&decl_ast.inputs, body_hir.params) {
                        visitor.visit_param(param_ast, param_hir);
                    }
                    visit_matching_expr(visitor, expr_ast, &body_hir.value);
                }
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
            (ast::ExprKind::Range(Some(start_ast), Some(end_ast), ast::RangeLimits::Closed), hir::ExprKind::Call(path_expr_hir, [start_hir, end_hir])) => {
                if let hir::ExprKind::Path(qpath_hir) = &path_expr_hir.kind
                    && let hir::QPath::LangItem(hir::LangItem::RangeInclusiveNew, _, _) = qpath_hir
                {
                    visit_matching_expr(visitor, start_ast, start_hir);
                    visit_matching_expr(visitor, end_ast, end_hir);
                }
            }
            (ast::ExprKind::Range(start_ast, end_ast, limits), hir::ExprKind::Struct(qpath_hir, fields_hir, _)) => {
                let expected_lang_item = match (start_ast, end_ast, limits) {
                    (None, None, ast::RangeLimits::HalfOpen) => hir::LangItem::RangeFull,
                    (Some(_), None, ast::RangeLimits::HalfOpen) => hir::LangItem::RangeFrom,
                    (None, Some(_), ast::RangeLimits::HalfOpen) => hir::LangItem::RangeTo,
                    (Some(_), Some(_), ast::RangeLimits::HalfOpen) => hir::LangItem::Range,
                    (None, Some(_), ast::RangeLimits::Closed) => hir::LangItem::RangeToInclusive,
                    _ => unreachable!(),
                };

                if let hir::QPath::LangItem(lang_item_hir, _, _) = qpath_hir && *lang_item_hir == expected_lang_item {
                    let mut fields_hir_iter = fields_hir.iter();
                    if let Some(start_ast) = start_ast && let Some(start_field_hir) = fields_hir_iter.next() {
                        visit_matching_expr(visitor, start_ast, start_field_hir.expr);
                    }
                    if let Some(end_ast) = end_ast && let Some(end_field_hir) = fields_hir_iter.next() {
                        visit_matching_expr(visitor, end_ast, end_field_hir.expr);
                    }
                }
            }
            (ast::ExprKind::Underscore, _) => {}
            (ast::ExprKind::Path(_, _), hir::ExprKind::Path(_)) => {
                // TODO
            }
            (ast::ExprKind::AddrOf(_, _, expr_ast), hir::ExprKind::AddrOf(_, _, expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Break(_, expr_ast), hir::ExprKind::Break(_, expr_hir)) => {
                if let Some(expr_ast) = expr_ast && let Some(expr_hir) = expr_hir {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
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
            (ast::ExprKind::Struct(struct_ast), hir::ExprKind::Struct(_, fields_hir, base_hir)) => {
                // TODO
                for (field_ast, field_hir) in iter::zip(&struct_ast.fields, *fields_hir) {
                    // TODO
                    visit_matching_expr(visitor, &field_ast.expr, field_hir.expr);
                }
                if let ast::StructRest::Base(base_ast) = &struct_ast.rest && let Some(base_hir) = base_hir {
                    visit_matching_expr(visitor, base_ast, base_hir);
                }
            }
            (ast::ExprKind::Repeat(expr_ast, _anon_const_ast), hir::ExprKind::Repeat(expr_hir, _anon_const_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                // TODO: Visit anonymous const
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

#[derive(Copy, Clone, Debug)]
pub enum DefItem<'ast> {
    Item(&'ast ast::Item),
    ForeignItem(&'ast ast::ForeignItem),
    AssocItem(&'ast ast::AssocItem, ast::visit::AssocCtxt),
}

impl<'ast> DefItem<'ast> {
    pub fn ident(&self) -> Ident {
        match self {
            Self::Item(item) => item.ident,
            Self::ForeignItem(item) => item.ident,
            Self::AssocItem(item, _) => item.ident,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Item(item) => item.span,
            Self::ForeignItem(item) => item.span,
            Self::AssocItem(item, _) => item.span,
        }
    }

    pub fn kind(&self) -> ast::ItemKind {
        match self {
            Self::Item(item) => item.kind.clone(),
            Self::ForeignItem(item) => item.kind.clone().into(),
            Self::AssocItem(item, _) => item.kind.clone().into(),
        }
    }
}

struct AstDefFinder<'ast, 'hir> {
    path_stack: VecDeque<hir::Node<'hir>>,
    result: Option<DefItem<'ast>>,
}

fn is_matching_item_kind<'ast, 'hir>(item_kind_ast: &'ast ast::ItemKind, item_kind_hir: &'hir hir::ItemKind) -> bool {
    match (item_kind_hir, item_kind_ast) {
        (hir::ItemKind::ExternCrate(symbol_hir), ast::ItemKind::ExternCrate(symbol_ast)) => {
            symbol_ast == symbol_hir
        }
        (hir::ItemKind::Use(_, _), _) => false,
        (hir::ItemKind::Static(_, _, _), ast::ItemKind::Static(_, _, _)) => true,
        (hir::ItemKind::Const(_, _), ast::ItemKind::Const(_, _, _)) => true,
        (hir::ItemKind::Fn(_, _, _), ast::ItemKind::Fn(_)) => true,
        (hir::ItemKind::Macro(def_hir, _), ast::ItemKind::MacroDef(def_ast)) => {
            std::ptr::eq(def_hir, def_ast)
        }
        (hir::ItemKind::Mod(_), ast::ItemKind::Mod(_, _)) => true,
        (hir::ItemKind::ForeignMod { abi: _, items: _ }, ast::ItemKind::ForeignMod(_)) => true,
        (hir::ItemKind::GlobalAsm(_), ast::ItemKind::GlobalAsm(_)) => true,
        (hir::ItemKind::TyAlias(_, _), ast::ItemKind::TyAlias(_)) => true,
        (hir::ItemKind::OpaqueTy(_), _) => false,
        (hir::ItemKind::Enum(_, _), ast::ItemKind::Enum(_, _)) => true,
        (hir::ItemKind::Struct(_, _), ast::ItemKind::Struct(_, _)) => true,
        (hir::ItemKind::Union(_, _), ast::ItemKind::Union(_, _)) => true,
        (hir::ItemKind::Trait(_, _, _, _, _), ast::ItemKind::Trait(_)) => true,
        (hir::ItemKind::TraitAlias(_, _), ast::ItemKind::TraitAlias(_, _)) => true,
        (hir::ItemKind::Impl(_), ast::ItemKind::Impl(_)) => true,

        _ => false,
    }
}

fn is_matching_foreign_item_kind<'ast, 'hir>(item_kind_ast: &'ast ast::ForeignItemKind, item_kind_hir: &'hir hir::ForeignItemKind) -> bool {
    match (item_kind_hir, item_kind_ast) {
        (hir::ForeignItemKind::Fn(_, _, _), ast::ForeignItemKind::Fn(_)) => true,
        (hir::ForeignItemKind::Static(_, _), ast::ForeignItemKind::Static(_, _, _)) => true,
        (hir::ForeignItemKind::Type, ast::ForeignItemKind::TyAlias(_)) => true,

        _ => false,
    }
}

fn is_matching_trait_item_kind<'ast, 'hir>(item_kind_ast: &'ast ast::AssocItemKind, item_kind_hir: &'hir hir::TraitItemKind) -> bool {
    match (item_kind_hir, item_kind_ast) {
        (hir::TraitItemKind::Const(_, _), ast::AssocItemKind::Const(_, _, _)) => true,
        (hir::TraitItemKind::Fn(_, _), ast::AssocItemKind::Fn(_)) => true,
        (hir::TraitItemKind::Type(_, _), ast::AssocItemKind::TyAlias(_)) => true,

        _ => false,
    }
}

fn is_matching_impl_item_kind<'ast, 'hir>(item_kind_ast: &'ast ast::AssocItemKind, item_kind_hir: &'hir hir::ImplItemKind) -> bool {
    match (item_kind_hir, item_kind_ast) {
        (hir::ImplItemKind::Const(_, _), ast::AssocItemKind::Const(_, _, _)) => true,
        (hir::ImplItemKind::Fn(_, _), ast::AssocItemKind::Fn(_)) => true,
        (hir::ImplItemKind::TyAlias(_), ast::AssocItemKind::TyAlias(_)) => true,

        _ => false,
    }
}

impl<'ast, 'hir> ast::visit::Visitor<'ast> for AstDefFinder<'ast, 'hir> {
    fn visit_crate(&mut self, krate: &'ast ast::Crate) {
        let Some(&path_segment) = self.path_stack.front() else { return; };

        if let hir::Node::Crate(_) = path_segment {
            self.path_stack.pop_front();
        }

        ast::visit::walk_crate(self, krate);
    }

    fn visit_item(&mut self, item: &'ast ast::Item) {
        let Some(path_segment) = self.path_stack.front() else { return; };

        match path_segment {
            hir::Node::Item(item_hir) => {
                if item.ident.name != item_hir.ident.name { return; }
                if !is_matching_item_kind(&item.kind, &item_hir.kind) { return; }
                self.path_stack.pop_front();
            }

            _ => { return; }
        }

        if self.path_stack.is_empty() {
            self.result = Some(DefItem::Item(item));
            return;
        }

        ast::visit::walk_item(self, item);
    }

    fn visit_foreign_item(&mut self, item: &'ast ast::ForeignItem) {
        let Some(path_segment) = self.path_stack.front() else { return; };

        match path_segment {
            hir::Node::ForeignItem(item_hir) => {
                if item.ident.name != item_hir.ident.name { return; }
                if !is_matching_foreign_item_kind(&item.kind, &item_hir.kind) { return; }
                self.path_stack.pop_front();
            }

            _ => { return; }
        }

        if self.path_stack.is_empty() {
            self.result = Some(DefItem::ForeignItem(item));
            return;
        }

        ast::visit::walk_foreign_item(self, item);
    }

    fn visit_assoc_item(&mut self, item: &'ast ast::AssocItem, ctx: ast::visit::AssocCtxt) {
        let Some(path_segment) = self.path_stack.front() else { return; };

        match (path_segment, ctx) {
            (hir::Node::TraitItem(item_hir), ast::visit::AssocCtxt::Trait) => {
                if item.ident.name != item_hir.ident.name { return; }
                if !is_matching_trait_item_kind(&item.kind, &item_hir.kind) { return; }
                self.path_stack.pop_front();
            }
            (hir::Node::ImplItem(item_hir), ast::visit::AssocCtxt::Impl) => {
                if item.ident.name != item_hir.ident.name { return; }
                if !is_matching_impl_item_kind(&item.kind, &item_hir.kind) { return; }
                self.path_stack.pop_front();
            }

            _ => { return; }
        }

        if self.path_stack.is_empty() {
            self.result = Some(DefItem::AssocItem(item, ctx));
            return;
        }

        ast::visit::walk_assoc_item(self, item, ctx);
    }
}

pub fn find_def_in_ast<'ast, 'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::LocalDefId, krate: &'ast ast::Crate) -> Option<DefItem<'ast>> {
    let def_hir_path = res::def_hir_path(tcx, def_id);
    let def_hir_node_path = def_hir_path.into_iter().map(|(_, node)| node).collect::<VecDeque<_>>();

    let mut finder = AstDefFinder {
        path_stack: def_hir_node_path,
        result: None,
    };
    finder.visit_crate(krate);

    finder.result
}
