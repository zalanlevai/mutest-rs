use std::collections::VecDeque;
use std::iter;
use std::ops::Deref;

use itertools::Itertools;
use rustc_data_structures::sync::HashMapExt;
use rustc_hash::FxHashMap;
use rustc_middle::span_bug;
use rustc_middle::ty::ResolverAstLowering;

use crate::analysis::hir;
use crate::analysis::ty::TyCtxt;
use crate::analysis::res;
use crate::codegen::ast;
use crate::codegen::symbols::{DUMMY_SP, Span};

pub struct DefResolutions {
    pub node_id_to_def_id: ast::node_id::NodeMap<hir::LocalDefId>,
    pub partial_res_map: ast::node_id::NodeMap<hir::PartialRes>,
    pub import_res_map: ast::node_id::NodeMap<hir::PerNS<Option<hir::Res<ast::NodeId>>>>,
}

impl DefResolutions {
    pub fn from_resolver(resolver: &ResolverAstLowering) -> Self {
        Self {
            node_id_to_def_id: resolver.node_id_to_def_id.clone(),
            partial_res_map: resolver.partial_res_map.clone(),
            import_res_map: resolver.import_res_map.clone(),
        }
    }

    pub fn node_res(&self, node_id: ast::NodeId) -> Option<hir::Res<ast::NodeId>> {
        self.partial_res_map.get(&node_id).and_then(|partial_res| partial_res.full_res())
    }

    pub fn import_res(&self, node_id: ast::NodeId) -> Option<hir::PerNS<Option<hir::Res<ast::NodeId>>>> {
        self.import_res_map.get(&node_id).copied()
    }
}

pub mod visit {
    use std::assert_matches::assert_matches;
    use std::iter;

    use rustc_abi::ExternAbi;
    use rustc_hir::intravisit::nested_filter::{self, NestedFilter};
    use rustc_middle::ty::TyCtxt;
    use rustc_span::Span;
    use rustc_span::symbol::kw;

    use crate::analysis::Descr;
    use crate::analysis::hir;
    use crate::codegen::ast;

    pub trait AstHirVisitor<'ast, 'hir>: Sized {
        type NestedFilter: NestedFilter<'hir> = nested_filter::None;

        fn tcx(&mut self) -> TyCtxt<'hir>;

        fn def_res(&mut self) -> &super::DefResolutions;

        fn nested_item(&mut self, id: hir::ItemId) -> Option<&'hir hir::Item<'hir>> {
            Self::NestedFilter::INTER.then(|| self.tcx().hir_item(id))
        }

        fn nested_trait_item(&mut self, id: hir::TraitItemId) -> Option<&'hir hir::TraitItem<'hir>> {
            Self::NestedFilter::INTER.then(|| self.tcx().hir_trait_item(id))
        }

        fn nested_impl_item(&mut self, id: hir::ImplItemId) -> Option<&'hir hir::ImplItem<'hir>> {
            Self::NestedFilter::INTER.then(|| self.tcx().hir_impl_item(id))
        }

        fn nested_foreign_item(&mut self, id: hir::ForeignItemId) -> Option<&'hir hir::ForeignItem<'hir>> {
            Self::NestedFilter::INTER.then(|| self.tcx().hir_foreign_item(id))
        }

        fn nested_body(&mut self, id: hir::BodyId) -> Option<&'hir hir::Body<'hir>> {
            Self::NestedFilter::INTRA.then(|| self.tcx().hir_body(id))
        }

        fn visit_fn_item(&mut self, fn_ast: &ast::FnItem<'ast>, fn_hir: &hir::FnItem<'hir>) {
            let kind_ast = ast::visit::FnKind::Fn(fn_ast.ctx, &fn_ast.vis, fn_ast.fn_data);
            let coroutine_kind_ast = &fn_ast.fn_data.sig.header.coroutine_kind;
            let span_ast = fn_ast.span;
            let id_ast = fn_ast.id;
            let kind_hir = fn_hir.kind;
            let generics_hir = Some(fn_hir.generics);
            let sig_hir = *fn_hir.sig;
            let body_hir = fn_hir.body.map(|body| body.id());
            let span_hir = fn_hir.span;
            let id_hir = self.tcx().local_def_id_to_hir_id(fn_hir.owner_id.def_id);
            self.visit_fn(kind_ast, coroutine_kind_ast, span_ast, id_ast, kind_hir, generics_hir, sig_hir, body_hir, span_hir, id_hir);
        }

        fn visit_fn(&mut self, kind_ast: ast::visit::FnKind<'ast>, coroutine_kind_ast: &'ast Option<ast::CoroutineKind>, span_ast: Span, id_ast: ast::NodeId, kind_hir: hir::intravisit::FnKind<'hir>, generics_hir: Option<&'hir hir::Generics<'hir>>, sig_hir: hir::FnSig<'hir>, body_hir: Option<hir::BodyId>, span_hir: Span, id_hir: hir::HirId) {
            walk_fn(self, kind_ast, coroutine_kind_ast, span_ast, id_ast, kind_hir, generics_hir, sig_hir, body_hir, span_hir, id_hir);
        }

        fn visit_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>, param_ty_hir: &'hir hir::Ty<'hir>) {
            walk_param(self, param_ast, param_hir, param_ty_hir);
        }

        fn visit_coroutine_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>, param_pat_hir: &'hir hir::Pat<'hir>, param_ty_hir: &'hir hir::Ty<'hir>) {
            walk_coroutine_param(self, param_ast, param_hir, param_pat_hir, param_ty_hir);
        }

        fn visit_coroutine_return_impl_trait(&mut self, coroutine_kind_ast: &'ast ast::CoroutineKind, ret_ty_ast: &'ast ast::FnRetTy, ret_ty_hir: &'hir hir::Ty<'hir>) {
            walk_coroutine_return_impl_trait(self, coroutine_kind_ast, ret_ty_ast, ret_ty_hir);
        }

        fn visit_const(&mut self, const_ast: &'ast ast::ConstItem, const_hir: &hir::ConstItem<'hir>) {
            walk_const(self, const_ast, const_hir);
        }

        fn visit_static(&mut self, static_ast: &'ast ast::StaticItem, static_hir: &hir::StaticItem<'hir>) {
            walk_static(self, static_ast, static_hir);
        }

        fn visit_enum(&mut self, enum_def_ast: &'ast ast::EnumDef, generics_ast: &'ast ast::Generics, enum_def_hir: &'hir hir::EnumDef<'hir>, generics_hir: &'hir hir::Generics<'hir>) {
            walk_enum(self, enum_def_ast, generics_ast, enum_def_hir, generics_hir);
        }

        fn visit_struct(&mut self, variant_data_ast: &'ast ast::VariantData, generics_ast: &'ast ast::Generics, variant_data_hir: &'hir hir::VariantData<'hir>, generics_hir: &'hir hir::Generics<'hir>) {
            walk_struct(self, variant_data_ast, generics_ast, variant_data_hir, generics_hir);
        }

        fn visit_variant(&mut self, variant_ast: &'ast ast::Variant, variant_hir: &'hir hir::Variant<'hir>) {
            walk_variant(self, variant_ast, variant_hir);
        }

        fn visit_variant_data(&mut self, variant_data_ast: &'ast ast::VariantData, variant_data_hir: &'hir hir::VariantData<'hir>) {
            walk_variant_data(self, variant_data_ast, variant_data_hir);
        }

        fn visit_field_def(&mut self, field_def_ast: &'ast ast::FieldDef, field_def_hir: &'hir hir::FieldDef<'hir>) {
            walk_field_def(self, field_def_ast, field_def_hir);
        }

        fn visit_ty_alias(&mut self, ty_alias_ast: &'ast ast::TyAlias, ty_alias_hir: &hir::TyAliasItem<'hir>) {
            walk_ty_alias(self, ty_alias_ast, ty_alias_hir);
        }

        fn visit_trait(&mut self, trait_ast: &'ast ast::Trait, generics_hir: &'hir hir::Generics<'hir>, generic_bounds_hir: hir::GenericBounds<'hir>) {
            walk_trait(self, trait_ast, generics_hir, generic_bounds_hir);
        }

        fn visit_trait_alias(&mut self, generics_ast: &'ast ast::Generics, generic_bounds_ast: &'ast ast::GenericBounds, generics_hir: &'hir hir::Generics<'hir>, generic_bounds_hir: hir::GenericBounds<'hir>) {
            walk_trait_alias(self, generics_ast, generic_bounds_ast, generics_hir, generic_bounds_hir);
        }

        fn visit_impl(&mut self, impl_ast: &'ast ast::Impl, impl_hir: &'hir hir::Impl<'hir>) {
            walk_impl(self, impl_ast, impl_hir);
        }

        fn visit_poly_trait_ref(&mut self, poly_trait_ref_ast: &'ast ast::PolyTraitRef, poly_trait_ref_hir: &'hir hir::PolyTraitRef<'hir>) {
            walk_poly_trait_ref(self, poly_trait_ref_ast, poly_trait_ref_hir);
        }

        fn visit_trait_ref(&mut self, trait_ref_ast: &'ast ast::TraitRef, trait_ref_hir: &'hir hir::TraitRef<'hir>) {
            walk_trait_ref(self, trait_ref_ast, trait_ref_hir);
        }

        fn visit_generics(&mut self, generics_ast: &'ast ast::Generics, generics_hir: &'hir hir::Generics<'hir>) {
            walk_generics(self, generics_ast, generics_hir);
        }

        fn visit_generic_param(&mut self, generic_param_ast: &'ast ast::GenericParam, generic_param_hir: &'hir hir::GenericParam<'hir>) {
            walk_generic_param(self, generic_param_ast, generic_param_hir);
        }

        fn visit_generic_bound(&mut self, generic_bound_ast: &'ast ast::GenericBound, generic_bound_hir: &'hir hir::GenericBound<'hir>) {
            walk_generic_bound(self, generic_bound_ast, generic_bound_hir);
        }

        fn visit_block(&mut self, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
            walk_block(self, block_ast, block_hir);
        }

        fn visit_stmt(&mut self, stmt_ast: &'ast ast::Stmt, stmt_hir: &'hir hir::Stmt<'hir>) {
            walk_stmt(self, stmt_ast, stmt_hir);
        }

        fn visit_local(&mut self, local_ast: &'ast ast::Local, local_hir: &'hir hir::LetStmt<'hir>) {
            walk_local(self, local_ast, local_hir);
        }

        fn visit_expr(&mut self, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
            walk_expr(self, expr_ast, expr_hir);
        }

        fn visit_expr_field(&mut self, expr_field_ast: &'ast ast::ExprField, expr_field_hir: &'hir hir::ExprField<'hir>) {
            walk_expr_field(self, expr_field_ast, expr_field_hir);
        }

        fn visit_arm(&mut self, arm_ast: &'ast ast::Arm, arm_hir: &'hir hir::Arm<'hir>) {
            walk_arm(self, arm_ast, arm_hir);
        }

        fn visit_pat(&mut self, pat_ast: &'ast ast::Pat, pat_hir: &'hir hir::Pat<'hir>) {
            walk_pat(self, pat_ast, pat_hir);
        }

        fn visit_pat_expr(&mut self, pat_expr_ast: &'ast ast::Expr, pat_expr_hir: &'hir hir::PatExpr<'hir>) {
            walk_pat_expr(self, pat_expr_ast, pat_expr_hir);
        }

        fn visit_pat_field(&mut self, pat_field_ast: &'ast ast::PatField, pat_field_hir: &'hir hir::PatField<'hir>) {
            walk_pat_field(self, pat_field_ast, pat_field_hir);
        }

        fn visit_ty(&mut self, ty_ast: &'ast ast::Ty, ty_hir: &'hir hir::Ty<'hir>) {
            walk_ty(self, ty_ast, ty_hir);
        }

        fn visit_ty_pat(&mut self, ty_pat_ast: &'ast ast::TyPat, ty_pat_hir: &'hir hir::TyPat<'hir>) {
            walk_ty_pat(self, ty_pat_ast, ty_pat_hir);
        }

        fn visit_impl_trait(&mut self, node_id: ast::NodeId, generic_bounds_ast: &'ast ast::GenericBounds, opaque_ty_hir: &'hir hir::OpaqueTy<'hir>) {
            walk_impl_trait(self, node_id, generic_bounds_ast, opaque_ty_hir);
        }

        fn visit_impl_trait_in_param(&mut self, node_id: ast::NodeId, generic_bounds_ast: &'ast ast::GenericBounds, impl_trait_predicate_hir_id: hir::HirId, impl_trait_predicate_hir: &'hir hir::WhereBoundPredicate<'hir>) {
            walk_impl_trait_in_param(self, node_id, generic_bounds_ast, impl_trait_predicate_hir_id, impl_trait_predicate_hir);
        }

        fn visit_path(&mut self, path_ast: &'ast ast::Path, path_hir: &'hir hir::Path<'hir>) {
            walk_path(self, path_ast, path_hir);
        }

        fn visit_qpath(&mut self, qself_ast: Option<&'ast ast::QSelf>, path_ast: &'ast ast::Path, qpath_hir: &'hir hir::QPath<'hir>) {
            walk_qpath(self, qself_ast, path_ast, qpath_hir);
        }

        fn visit_path_segment(&mut self, path_segment_ast: &'ast ast::PathSegment, path_segment_hir: &'hir hir::PathSegment<'hir>) {
            walk_path_segment(self, path_segment_ast, path_segment_hir);
        }

        fn visit_generic_args(&mut self, generic_args_ast: &'ast ast::GenericArgs, generic_args_hir: &'hir hir::GenericArgs<'hir>) {
            walk_generic_args(self, generic_args_ast, generic_args_hir);
        }

        fn visit_generic_arg(&mut self, generic_arg_ast: &'ast ast::GenericArg, generic_arg_hir: &'hir hir::GenericArg<'hir>) {
            walk_generic_arg(self, generic_arg_ast, generic_arg_hir);
        }

        fn visit_assoc_item_constraint(&mut self, constraint_ast: &'ast ast::AssocItemConstraint, constraint_hir: &'hir hir::AssocItemConstraint<'hir>) {
            walk_assoc_item_constraint(self, constraint_ast, constraint_hir);
        }

        fn visit_anon_const(&mut self, anon_const_ast: &'ast ast::AnonConst, anon_const_hir: &'hir hir::AnonConst) {
            walk_anon_const(self, anon_const_ast, anon_const_hir);
        }

        fn visit_path_anon_const(&mut self, ty_ast: &'ast ast::Ty, const_arg_hir: &'hir hir::ConstArg<'hir>) {
            walk_path_anon_const(self, ty_ast, const_arg_hir);
        }

        fn visit_const_block(&mut self, anon_const_ast: &'ast ast::AnonConst, const_block_hir: &'hir hir::ConstBlock) {
            walk_const_block(self, anon_const_ast, const_block_hir);
        }

        fn visit_const_arg(&mut self, anon_const_ast: &'ast ast::AnonConst, const_arg_hir: &'hir hir::ConstArg<'hir>) {
            walk_const_arg(self, anon_const_ast, const_arg_hir);
        }
    }

    struct CoroutineHirBody<'hir> {
        pub param_assign_stmts: &'hir [hir::Stmt<'hir>],
        pub body_expr: &'hir hir::Expr<'hir>,
    }

    fn coroutine_hir_body<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, body_hir: &'hir hir::Body<'hir>) -> Option<CoroutineHirBody<'hir>> {
        let hir::ExprKind::Closure(coroutine_closure_hir) = body_hir.value.kind else { return None; };
        let coroutine_closure_body_hir = visitor.nested_body(coroutine_closure_hir.body)?;

        let hir::ExprKind::Block(coroutine_closure_block_hir, None) = coroutine_closure_body_hir.value.kind else { return None; };
        let param_assign_stmts_hir = coroutine_closure_block_hir.stmts;
        let Some(hir::Expr { kind: hir::ExprKind::DropTemps(body_expr_hir), .. }) = coroutine_closure_block_hir.expr else { return None; };

        Some(CoroutineHirBody {
            param_assign_stmts: param_assign_stmts_hir,
            body_expr: body_expr_hir,
        })
    }

    pub fn walk_fn<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, kind_ast: ast::visit::FnKind<'ast>, coroutine_kind_ast: &'ast Option<ast::CoroutineKind>, _span_ast: Span, _id_ast: ast::NodeId, kind_hir: hir::intravisit::FnKind<'hir>, generics_hir: Option<&'hir hir::Generics<'hir>>, sig_hir: hir::FnSig<'hir>, body_hir: Option<hir::BodyId>, _span_hir: Span, _id_hir: hir::HirId) {
        match (kind_ast, kind_hir) {
            | (ast::visit::FnKind::Fn(_, _, fn_data_ast), hir::intravisit::FnKind::ItemFn(_, _, _))
            | (ast::visit::FnKind::Fn(_, _, fn_data_ast), hir::intravisit::FnKind::Method(_, _)) => {
                if let Some(generics_hir) = generics_hir {
                    visitor.visit_generics(&fn_data_ast.generics, generics_hir);
                }

                let body_hir = body_hir.and_then(|body_hir| visitor.nested_body(body_hir));

                match (&fn_data_ast.sig.header.coroutine_kind, sig_hir.header.asyncness) {
                    (None, hir::IsAsync::NotAsync) => {
                        if let Some(body_hir) = body_hir {
                            for (param_ast, (param_hir, param_hir_ty)) in iter::zip(&fn_data_ast.sig.decl.inputs, iter::zip(body_hir.params, sig_hir.decl.inputs)) {
                                visitor.visit_param(param_ast, param_hir, param_hir_ty);
                            }
                        } else {
                            for (param_ast, param_hir_ty) in iter::zip(&fn_data_ast.sig.decl.inputs, sig_hir.decl.inputs) {
                                visitor.visit_ty(&param_ast.ty, param_hir_ty);
                            }
                        }

                        match (&fn_data_ast.sig.decl.output, sig_hir.decl.output) {
                            (ast::FnRetTy::Default(_), hir::FnRetTy::DefaultReturn(_)) => {}
                            (ast::FnRetTy::Ty(ret_ty_ast), hir::FnRetTy::Return(ret_ty_hir)) => {
                                visit_matching_ty(visitor, &ret_ty_ast, ret_ty_hir);
                            }
                            _ => unreachable!(),
                        }

                        if let Some(body_hir) = body_hir && let Some(body_ast) = &fn_data_ast.body {
                            visit_block_expr(visitor, body_ast, &body_hir.value);
                        }
                    }

                    (Some(coroutine_kind_ast @ ast::CoroutineKind::Async { span: _, closure_id: _, return_impl_trait_id: _ }), hir::IsAsync::Async(_)) => {
                        let coroutine_body_hir = body_hir.map(|body_hir| {
                            let Some(coroutine_body_hir) = coroutine_hir_body(visitor, body_hir) else { unreachable!() };
                            coroutine_body_hir
                        });

                        if let Some(body_hir) = body_hir && let Some(coroutine_body_hir) = &coroutine_body_hir {
                            walk_coroutine_params(visitor, &fn_data_ast.sig.decl.inputs, body_hir.params, sig_hir.decl.inputs, coroutine_body_hir);
                        } else {
                            for (param_ast, param_hir_ty) in iter::zip(&fn_data_ast.sig.decl.inputs, sig_hir.decl.inputs) {
                                visitor.visit_ty(&param_ast.ty, param_hir_ty);
                            }
                        }

                        if let hir::FnRetTy::Return(ret_ty_hir) = sig_hir.decl.output {
                            visitor.visit_coroutine_return_impl_trait(coroutine_kind_ast, &fn_data_ast.sig.decl.output, ret_ty_hir);
                        } else { unreachable!() }

                        if let Some(body_ast) = &fn_data_ast.body && let Some(coroutine_body_hir) = &coroutine_body_hir {
                            visit_block_expr(visitor, body_ast, coroutine_body_hir.body_expr);
                        }
                    }

                    (Some(ast::CoroutineKind::Gen { span: _, closure_id: _, return_impl_trait_id: _ }), hir::IsAsync::NotAsync) => {}
                    (Some(ast::CoroutineKind::AsyncGen { span: _, closure_id: _, return_impl_trait_id: _ }), hir::IsAsync::NotAsync) => {}

                    _ => unreachable!(),
                }
            }
            (ast::visit::FnKind::Closure(_, _, decl_ast, expr_ast), hir::intravisit::FnKind::Closure) => {
                let body_hir = body_hir.and_then(|body_hir| visitor.nested_body(body_hir));

                if let Some(body_hir) = body_hir {
                    match (coroutine_kind_ast, sig_hir.header.asyncness) {
                        (None, hir::IsAsync::NotAsync) => {
                            for (param_ast, (param_hir, param_hir_ty)) in iter::zip(&decl_ast.inputs, iter::zip(body_hir.params, sig_hir.decl.inputs)) {
                                visitor.visit_param(param_ast, param_hir, param_hir_ty);
                            }

                            match (&decl_ast.output, sig_hir.decl.output) {
                                (ast::FnRetTy::Default(_), hir::FnRetTy::DefaultReturn(_)) => {}
                                (ast::FnRetTy::Ty(ret_ty_ast), hir::FnRetTy::Return(ret_ty_hir)) => {
                                    visit_matching_ty(visitor, &ret_ty_ast, ret_ty_hir);
                                }
                                _ => unreachable!(),
                            }

                            visit_matching_expr(visitor, expr_ast, &body_hir.value);
                        }

                        (Some(ast::CoroutineKind::Async { span: _, closure_id: _, return_impl_trait_id: _ }), hir::IsAsync::Async(_)) => {
                            let Some(coroutine_body_hir) = coroutine_hir_body(visitor, body_hir) else { unreachable!() };

                            walk_coroutine_params(visitor, &decl_ast.inputs, body_hir.params, sig_hir.decl.inputs, &coroutine_body_hir);

                            // NOTE: Closure return types are not wrapped in an `impl Trait` in the HIR.
                            if let ast::FnRetTy::Ty(ret_ty_ast) = &decl_ast.output && let hir::FnRetTy::Return(ret_ty_hir) = sig_hir.decl.output {
                                visit_matching_ty(visitor, &ret_ty_ast, ret_ty_hir);
                            }

                            visit_matching_expr(visitor, expr_ast, coroutine_body_hir.body_expr);
                        }

                        (Some(ast::CoroutineKind::Gen { span: _, closure_id: _, return_impl_trait_id: _ }), hir::IsAsync::NotAsync) => {}
                        (Some(ast::CoroutineKind::AsyncGen { span: _, closure_id: _, return_impl_trait_id: _ }), hir::IsAsync::NotAsync) => {}

                        _ => unreachable!(),
                    }
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn walk_param<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>, param_ty_hir: &'hir hir::Ty<'hir>) {
        visit_matching_pat(visitor, &param_ast.pat, param_hir.pat);
        visit_matching_ty(visitor, &param_ast.ty, param_ty_hir);
    }

    fn walk_coroutine_params<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, params_ast: &'ast [ast::Param], params_hir: &'hir [hir::Param<'hir>], param_tys_hir: &'hir [hir::Ty<'hir>], coroutine_hir_body: &CoroutineHirBody<'hir>) {
        let mut param_assign_stmt_hir_iter = coroutine_hir_body.param_assign_stmts.iter();

        for (param_ast, (param_hir, param_ty_hir)) in iter::zip(params_ast, iter::zip(params_hir, param_tys_hir)) {

            // TODO: Reference `lower_coroutine_body_with_moved_arguments` and `lower_pat_ident`.
            let is_simple_parameter = match &param_ast.pat.kind {
                ast::PatKind::Ident(ast::BindingMode(ast::ByRef::No, _), _, _)
                    if let None | Some(hir::Res::Local(_)) = visitor.def_res().node_res(param_ast.pat.id)
                => true,
                _ => false,
            };

            let Some(param_assign_stmt_hir) = (match is_simple_parameter {
                true => param_assign_stmt_hir_iter.next(),
                false => {
                    let _ = param_assign_stmt_hir_iter.next();
                    param_assign_stmt_hir_iter.next()
                }
            }) else { unreachable!() };
            let hir::StmtKind::Let(param_assign_let_stmt_hir) = param_assign_stmt_hir.kind else { unreachable!() };

            visitor.visit_coroutine_param(param_ast, param_hir, &param_assign_let_stmt_hir.pat, param_ty_hir);
        }
    }

    pub fn walk_coroutine_param<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, param_ast: &'ast ast::Param, _param_hir: &'hir hir::Param<'hir>, param_pat_hir: &'hir hir::Pat<'hir>, param_ty_hir: &'hir hir::Ty<'hir>) {
        visit_matching_pat(visitor, &param_ast.pat, param_pat_hir);
        visit_matching_ty(visitor, &param_ast.ty, param_ty_hir);
    }

    pub fn walk_coroutine_return_impl_trait<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, _coroutine_kind_ast: &'ast ast::CoroutineKind, ret_ty_ast: &'ast ast::FnRetTy, ret_ty_hir: &'hir hir::Ty<'hir>) {
        if let hir::TyKind::OpaqueDef(opaque_ty) = ret_ty_hir.kind
            && let opaque_ty_hir = visitor.tcx().hir_node(opaque_ty.hir_id).expect_opaque_ty()
            && let hir::OpaqueTyOrigin::AsyncFn { .. } = opaque_ty_hir.origin
            && let [hir::GenericBound::Trait(poly_trait_ref_hir)] = opaque_ty_hir.bounds
            && let [trait_path_segment_hir] = poly_trait_ref_hir.trait_ref.path.segments
            && let Some(trait_generic_args_hir) = trait_path_segment_hir.args
            && let [trait_generic_arg_constraint_hir] = trait_generic_args_hir.constraints
            && let hir::AssocItemConstraintKind::Equality { term: hir::Term::Ty(ret_ty_hir) } = trait_generic_arg_constraint_hir.kind
        {
            match ret_ty_ast {
                ast::FnRetTy::Default(_) => {}
                ast::FnRetTy::Ty(ret_ty_ast) => {
                    visit_matching_ty(visitor, ret_ty_ast, ret_ty_hir);
                }
            }
        }
    }

    pub fn walk_const<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, const_ast: &'ast ast::ConstItem, const_hir: &hir::ConstItem<'hir>) {
        visit_matching_ty(visitor, &const_ast.ty, const_hir.ty);
        if let Some(generics_hir) = const_hir.generics {
            visitor.visit_generics(&const_ast.generics, generics_hir);
        }
        if let Some(expr_ast) = &const_ast.expr && let Some(body_hir) = const_hir.body {
            visit_matching_expr(visitor, expr_ast, &body_hir.value)
        }
    }

    pub fn walk_static<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, static_ast: &'ast ast::StaticItem, static_hir: &hir::StaticItem<'hir>) {
        visit_matching_ty(visitor, &static_ast.ty, static_hir.ty);
        if let Some(expr_ast) = &static_ast.expr && let Some(body_hir) = static_hir.body {
            visit_matching_expr(visitor, expr_ast, &body_hir.value)
        }
    }

    pub fn walk_enum<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, enum_def_ast: &'ast ast::EnumDef, generics_ast: &'ast ast::Generics, enum_def_hir: &'hir hir::EnumDef<'hir>, generics_hir: &'hir hir::Generics<'hir>) {
        visitor.visit_generics(generics_ast, generics_hir);
        for (variant_ast, variant_hir) in iter::zip(&enum_def_ast.variants, enum_def_hir.variants) {
            visitor.visit_variant(variant_ast, variant_hir);
        }
    }

    pub fn walk_struct<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, variant_data_ast: &'ast ast::VariantData, generics_ast: &'ast ast::Generics, variant_data_hir: &'hir hir::VariantData<'hir>, generics_hir: &'hir hir::Generics<'hir>) {
        visitor.visit_generics(generics_ast, generics_hir);
        visitor.visit_variant_data(variant_data_ast, variant_data_hir);
    }

    pub fn walk_variant<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, variant_ast: &'ast ast::Variant, variant_hir: &'hir hir::Variant<'hir>) {
        visitor.visit_variant_data(&variant_ast.data, &variant_hir.data);
        if let Some(anon_const_ast) = &variant_ast.disr_expr && let Some(anon_const_hir) = &variant_hir.disr_expr {
            visitor.visit_anon_const(anon_const_ast, anon_const_hir);
        }
    }

    pub fn walk_variant_data<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, variant_data_ast: &'ast ast::VariantData, variant_data_hir: &'hir hir::VariantData<'hir>) {
        for (field_def_ast, field_def_hir) in iter::zip(variant_data_ast.fields(), variant_data_hir.fields()) {
            visitor.visit_field_def(field_def_ast, field_def_hir);
        }
    }

    pub fn walk_field_def<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, field_def_ast: &'ast ast::FieldDef, field_def_hir: &'hir hir::FieldDef<'hir>) {
        visit_matching_ty(visitor, &field_def_ast.ty, field_def_hir.ty);
    }

    pub fn walk_ty_alias<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, ty_alias_ast: &'ast ast::TyAlias, ty_alias_hir: &hir::TyAliasItem<'hir>) {
        if let Some(ty_ast) = &ty_alias_ast.ty && let Some(ty_hir) = ty_alias_hir.ty() {
            visit_matching_ty(visitor, ty_ast, ty_hir);
        }

        match ty_alias_hir {
            hir::TyAliasItem::Item(generics_hir, _) => {
                visitor.visit_generics(&ty_alias_ast.generics, generics_hir);
            }
            hir::TyAliasItem::TraitItem(generic_bounds_hir, _) => {
                for (generic_bound_ast, generic_bound_hir) in iter::zip(&ty_alias_ast.bounds, *generic_bounds_hir) {
                    visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
                }
            }
            hir::TyAliasItem::ImplItem(_) => {}
        }
    }

    pub fn walk_trait<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, trait_ast: &'ast ast::Trait, generics_hir: &'hir hir::Generics<'hir>, generic_bounds_hir: hir::GenericBounds<'hir>) {
        visitor.visit_generics(&trait_ast.generics, generics_hir);
        for (generic_bound_ast, generic_bound_hir) in iter::zip(&trait_ast.bounds, generic_bounds_hir) {
            visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
        }
    }

    pub fn walk_trait_alias<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, generics_ast: &'ast ast::Generics, generic_bounds_ast: &'ast ast::GenericBounds, generics_hir: &'hir hir::Generics<'hir>, generic_bounds_hir: hir::GenericBounds<'hir>) {
        visitor.visit_generics(generics_ast, generics_hir);
        for (generic_bound_ast, generic_bound_hir) in iter::zip(generic_bounds_ast, generic_bounds_hir) {
            visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
        }
    }

    pub fn walk_impl<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, impl_ast: &'ast ast::Impl, impl_hir: &'hir hir::Impl<'hir>) {
        if let Some(trait_ref_ast) = &impl_ast.of_trait && let Some(trait_ref_hir) = &impl_hir.of_trait {
            visitor.visit_trait_ref(trait_ref_ast, trait_ref_hir);
        }
        visit_matching_ty(visitor, &impl_ast.self_ty, impl_hir.self_ty);
        visitor.visit_generics(&impl_ast.generics, impl_hir.generics);
    }

    pub fn walk_poly_trait_ref<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, poly_trait_ref_ast: &'ast ast::PolyTraitRef, poly_trait_ref_hir: &'hir hir::PolyTraitRef<'hir>) {
        for (generic_param_ast, generic_param_hir) in iter::zip(&poly_trait_ref_ast.bound_generic_params, poly_trait_ref_hir.bound_generic_params) {
            visitor.visit_generic_param(generic_param_ast, generic_param_hir);
        }
        visitor.visit_trait_ref(&poly_trait_ref_ast.trait_ref, &poly_trait_ref_hir.trait_ref);
    }

    pub fn walk_trait_ref<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, trait_ref_ast: &'ast ast::TraitRef, trait_ref_hir: &'hir hir::TraitRef<'hir>) {
        visitor.visit_path(&trait_ref_ast.path, trait_ref_hir.path);
    }

    pub fn walk_generics<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, generics_ast: &'ast ast::Generics, generics_hir: &'hir hir::Generics<'hir>) {
        let mut where_predicates_hir_iter = generics_hir.predicates.iter();

        for (generic_param_ast, generic_param_hir) in iter::zip(&generics_ast.params, generics_hir.params) {
            visitor.visit_generic_param(generic_param_ast, generic_param_hir);

            if !generic_param_ast.bounds.is_empty() {
                let Some(where_predicate_hir) = where_predicates_hir_iter.next() else { unreachable!() };
                match (&generic_param_ast.kind, where_predicate_hir.kind) {
                    (ast::GenericParamKind::Type { .. }, hir::WherePredicateKind::BoundPredicate(predicate_hir)) => {
                        assert_matches!(predicate_hir.origin, hir::PredicateOrigin::GenericParam);

                        for (generic_bound_ast, generic_bound_hir) in iter::zip(&generic_param_ast.bounds, predicate_hir.bounds) {
                            visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
                        }
                    }
                    (ast::GenericParamKind::Lifetime, hir::WherePredicateKind::RegionPredicate(predicate_hir)) => {
                        assert!(!predicate_hir.in_where_clause);

                        for (generic_bound_ast, generic_bound_hir) in iter::zip(&generic_param_ast.bounds, predicate_hir.bounds) {
                            visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }

        for (where_predicate_ast, where_predicate_hir) in iter::zip(&generics_ast.where_clause.predicates, where_predicates_hir_iter) {
            match (&where_predicate_ast.kind, where_predicate_hir.kind) {
                (ast::WherePredicateKind::BoundPredicate(predicate_ast), hir::WherePredicateKind::BoundPredicate(predicate_hir)) => {
                    assert_matches!(predicate_hir.origin, hir::PredicateOrigin::WhereClause);

                    visit_matching_ty(visitor, &predicate_ast.bounded_ty, predicate_hir.bounded_ty);
                    for (generic_param_ast, generic_param_hir) in iter::zip(&predicate_ast.bound_generic_params, predicate_hir.bound_generic_params) {
                        visitor.visit_generic_param(generic_param_ast, generic_param_hir);
                    }
                    for (generic_bound_ast, generic_bound_hir) in iter::zip(&predicate_ast.bounds, predicate_hir.bounds) {
                        visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
                    }
                }
                (ast::WherePredicateKind::RegionPredicate(predicate_ast), hir::WherePredicateKind::RegionPredicate(predicate_hir)) => {
                    assert!(predicate_hir.in_where_clause);

                    // TODO: Visit lifetime
                    for (generic_bound_ast, generic_bound_hir) in iter::zip(&predicate_ast.bounds, predicate_hir.bounds) {
                        visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
                    }
                }
                (ast::WherePredicateKind::EqPredicate(predicate_ast), hir::WherePredicateKind::EqPredicate(predicate_hir)) => {
                    visit_matching_ty(visitor, &predicate_ast.lhs_ty, predicate_hir.lhs_ty);
                    visit_matching_ty(visitor, &predicate_ast.rhs_ty, predicate_hir.rhs_ty);
                }
                _ => unreachable!(),
            }
        }

        // TODO: Handle HIR where predicates with `hir::PredicateOrigin::ImplTrait`.
    }

    pub fn walk_generic_param<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, generic_param_ast: &'ast ast::GenericParam, generic_param_hir: &'hir hir::GenericParam<'hir>) {
        match (&generic_param_ast.kind, generic_param_hir.kind) {
            (ast::GenericParamKind::Type { default: default_ty_ast }, hir::GenericParamKind::Type { default: default_ty_hir, .. }) => {
                if let Some(default_ty_ast) = default_ty_ast && let Some(default_ty_hir) = default_ty_hir {
                    visit_matching_ty(visitor, default_ty_ast, default_ty_hir);
                }
            }
            (ast::GenericParamKind::Lifetime, hir::GenericParamKind::Lifetime { .. }) => {}
            (ast::GenericParamKind::Const { ty: ty_ast, default: default_ast, .. }, hir::GenericParamKind::Const { ty: ty_hir, default: default_hir, .. }) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
                if let Some(default_ast) = default_ast && let Some(default_hir) = default_hir {
                    visitor.visit_const_arg(default_ast, default_hir);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn walk_generic_bound<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, generic_bound_ast: &'ast ast::GenericBound, generic_bound_hir: &'hir hir::GenericBound<'hir>) {
        match (generic_bound_ast, generic_bound_hir) {
            (ast::GenericBound::Trait(poly_trait_ref_ast), hir::GenericBound::Trait(poly_trait_ref_hir)) => {
                visitor.visit_poly_trait_ref(poly_trait_ref_ast, poly_trait_ref_hir);
            }
            (ast::GenericBound::Outlives(_lifetime_ast), hir::GenericBound::Outlives(_lifetime_hir)) => {
                // TODO: Visit lifetime
            }
            _ => unreachable!(),
        }
    }

    pub fn visit_block_expr<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, block_ast: &'ast ast::Block, expr_hir: &'hir hir::Expr<'hir>) {
        if let hir::ExprKind::Block(expr_hir_block, _) = expr_hir.kind {
            visitor.visit_block(block_ast, expr_hir_block);
        }
    }

    pub fn walk_block<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
        let block_ast_stmts = block_ast.stmts.iter()
            // These nodes do not exist in the HIR.
            .filter(|stmt| !matches!(stmt.kind, ast::StmtKind::Empty | ast::StmtKind::MacCall(_)))
            // Some item nodes are incompatible across the AST and the HIR, so we skip visiting them.
            .filter(|stmt| !matches!(stmt.kind, ast::StmtKind::Item(_)));
        let block_hir_stmts = block_hir.stmts.iter()
            // See above.
            .filter(|stmt| !matches!(stmt.kind, hir::StmtKind::Item(_)));

        for (stmt_ast, stmt_hir) in iter::zip(block_ast_stmts.clone(), block_hir_stmts) {
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

            (ast::StmtKind::Let(local_ast), hir::StmtKind::Let(local_hir)) => {
                visitor.visit_local(local_ast, local_hir);
            }

            (ast::StmtKind::Item(_item_ast), hir::StmtKind::Item(_item_hir)) => {
                // NOTE: Nested items are their own owner nodes, and so
                //       users should query these nodes separately.
            }

            (ast::StmtKind::Empty, _) | (ast::StmtKind::MacCall(_), _) => {}

            (ast_kind, hir_kind) => {
                let mut diagnostic = visitor.tcx().dcx().struct_warn("unrecognized AST-HIR node pair");
                diagnostic.span_note(stmt_ast.span, format!("AST node: {}", ast_kind.descr()));
                diagnostic.span_note(stmt_hir.span, format!("HIR node: {}", hir_kind.descr()));
                diagnostic.emit();
            }
        }
    }

    pub fn walk_local<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, local_ast: &'ast ast::Local, local_hir: &'hir hir::LetStmt<'hir>) {
        visit_matching_pat(visitor, &local_ast.pat, local_hir.pat);
        if let Some(ty_ast) = &local_ast.ty && let Some(ty_hir) = local_hir.ty {
            visit_matching_ty(visitor, ty_ast, ty_hir);
        }
        match (&local_ast.kind, local_hir.init, local_hir.els) {
            (ast::LocalKind::Init(expr_ast), Some(expr_hir), None) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::LocalKind::InitElse(expr_ast, els_ast), Some(expr_hir), Some(els_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                visitor.visit_block(els_ast, els_hir);
            }
            (ast::LocalKind::Decl, None, None) => {}
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
            (ast::ExprKind::Underscore, _) => {}
            (ast::ExprKind::MacCall(_), _) => {}
            (ast::ExprKind::Dummy, _) => {}
            (ast::ExprKind::Err(_), _) | (_, hir::ExprKind::Err(_)) => {}

            _ => visitor.visit_expr(expr_ast, expr_hir),
        }
    }

    /// Get the left-hand side subexpressions of an assignment expression that get corresponding assignments in the HIR.
    /// This is a simplified version of `rustc_ast_lowering::expr::{impl LoweringContext}::destructure_assign_mut`.
    pub fn assigned_subexprs_in_expr_assign_lhs<'ast>(def_res: &super::DefResolutions, expr: &'ast ast::Expr, out: &mut Vec<&'ast ast::Expr>) {
        match &expr.kind {
            ast::ExprKind::Underscore => {}
            ast::ExprKind::Paren(expr) => assigned_subexprs_in_expr_assign_lhs(def_res, expr, out),
            ast::ExprKind::Tup(exprs) | ast::ExprKind::Array(exprs) => {
                for expr in exprs {
                    if let ast::ExprKind::Range(None, None, ast::RangeLimits::HalfOpen) = expr.kind { continue; }
                    assigned_subexprs_in_expr_assign_lhs(def_res, expr, out);
                }
            }
            ast::ExprKind::Path(_, _) if let Some(res) = def_res.node_res(expr.id) && res.expected_in_unit_struct_pat() => {}
            ast::ExprKind::Call(_, args) => {
                for expr in args {
                    if let ast::ExprKind::Range(None, None, ast::RangeLimits::HalfOpen) = expr.kind { continue; }
                    assigned_subexprs_in_expr_assign_lhs(def_res, expr, out);
                }
            }
            ast::ExprKind::Struct(struct_expr) => {
                for field in &struct_expr.fields {
                    assigned_subexprs_in_expr_assign_lhs(def_res, &field.expr, out);
                }
            }
            _ => { out.push(expr); }
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
            (ast::ExprKind::Underscore, _) => {}
            (ast::ExprKind::MacCall(_), _) => {}
            (ast::ExprKind::Dummy, _) => {}
            (ast::ExprKind::Err(_), _) | (_, hir::ExprKind::Err(_)) => {}

            (ast::ExprKind::Array(exprs_ast), hir::ExprKind::Array(exprs_hir)) => {
                for (expr_ast, expr_hir) in iter::zip(exprs_ast, *exprs_hir) {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::ConstBlock(anon_const_ast), hir::ExprKind::ConstBlock(const_block_hir)) => {
                visitor.visit_const_block(anon_const_ast, const_block_hir);
            }
            (ast::ExprKind::Call(expr_ast, args_ast), hir::ExprKind::Call(expr_hir, args_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                for (arg_ast, arg_hir) in iter::zip(args_ast, *args_hir) {
                    visit_matching_expr(visitor, arg_ast, arg_hir);
                }
            }
            (ast::ExprKind::MethodCall(method_call_ast), hir::ExprKind::MethodCall(path_segment_hir, receiver_hir, args_hir, _)) => {
                let ast::MethodCall { seg: path_segment_ast, receiver: receiver_ast, args: args_ast, span: _ } = &**method_call_ast;

                visitor.visit_path_segment(path_segment_ast, path_segment_hir);
                visit_matching_expr(visitor, receiver_ast, receiver_hir);
                for (expr_ast, expr_hir) in iter::zip(args_ast, *args_hir) {
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
            (ast::ExprKind::Cast(expr_ast, ty_ast), hir::ExprKind::Cast(expr_hir, ty_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                visit_matching_ty(visitor, ty_ast, ty_hir);
            }
            (ast::ExprKind::Type(expr_ast, ty_ast), hir::ExprKind::Type(expr_hir, ty_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                visit_matching_ty(visitor, ty_ast, ty_hir);
            }
            (ast::ExprKind::Let(pat_ast, expr_ast, _, _), hir::ExprKind::Let(let_hir)) => {
                visit_matching_pat(visitor, pat_ast, let_hir.pat);
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
            (ast::ExprKind::ForLoop { pat: pat_ast, iter: iter_ast, body: body_ast, label: _, kind: kind_ast }, hir::ExprKind::Match(outer_match_expr_hir, [outer_match_arm_hir], hir::MatchSource::ForLoopDesugar)) => {
                if let hir::ExprKind::Loop(inner_loop_block_hir, _inner_loop_label_hir, hir::LoopSource::ForLoop, _) = outer_match_arm_hir.body.kind {
                    // TODO: Visit label
                    if let [inner_loop_block_stmt_hir] = inner_loop_block_hir.stmts
                        && let hir::StmtKind::Expr(inner_loop_block_expr_hir) = inner_loop_block_stmt_hir.kind
                        && let hir::ExprKind::Match(_, [_, inner_loop_match_some_arm_hir], hir::MatchSource::ForLoopDesugar) = inner_loop_block_expr_hir.kind
                    {
                        if let hir::PatKind::Struct(_, [inner_loop_match_some_arm_pat_field_hir], _) = inner_loop_match_some_arm_hir.pat.kind {
                            visit_matching_pat(visitor, pat_ast, inner_loop_match_some_arm_pat_field_hir.pat);
                        }
                        match kind_ast {
                            ast::ForLoopKind::For => {
                                if let hir::ExprKind::Call(_, [outer_match_expr_iter_hir]) = outer_match_expr_hir.kind {
                                    visit_matching_expr(visitor, iter_ast, outer_match_expr_iter_hir);
                                }
                            }
                            ast::ForLoopKind::ForAwait => {
                                // TODO
                            }
                        }
                        visit_block_expr(visitor, body_ast, inner_loop_match_some_arm_hir.body);
                    }
                }
            }
            (ast::ExprKind::Loop(block_ast, _, _), hir::ExprKind::Loop(block_hir, _, hir::LoopSource::Loop, _)) => {
                visitor.visit_block(block_ast, block_hir);
            }
            (ast::ExprKind::Match(expr_ast, arms_ast, _), hir::ExprKind::Match(expr_hir, arms_hir, hir::MatchSource::Normal)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                for (arm_ast, arm_hir) in iter::zip(arms_ast, *arms_hir) {
                    visitor.visit_arm(arm_ast, arm_hir);
                }
            }
            (ast::ExprKind::Closure(closure_ast), hir::ExprKind::Closure(closure_hir)) => {
                let ast::Closure { binder: binder_ast, capture_clause: _, constness: _, coroutine_kind: coroutine_kind_ast, movability: _, fn_decl: decl_ast, body: expr_ast, fn_decl_span: _, fn_arg_span: _ } = &**closure_ast;
                let hir::Closure { def_id: _, binder: _, constness: constness_hir, capture_clause: _, bound_generic_params: _, fn_decl: decl_hir, body: body_hir, fn_decl_span: decl_span_hir, fn_arg_span: _, kind: closure_kind_hir } = closure_hir;

                // TODO: Create separate `visit_closure` function that is more suited for closures.
                let kind_ast = ast::visit::FnKind::Closure(binder_ast, coroutine_kind_ast, decl_ast, expr_ast);
                let kind_hir = hir::intravisit::FnKind::Closure;
                let generics_hir = None;
                let sig_hir = hir::FnSig {
                    header: hir::FnHeader {
                        safety: hir::HeaderSafety::Normal(hir::Safety::Safe),
                        constness: *constness_hir,
                        asyncness: 'asyncness: {
                            let coroutine_desugaring = match closure_kind_hir {
                                hir::ClosureKind::Closure => break 'asyncness hir::IsAsync::NotAsync,
                                hir::ClosureKind::Coroutine(hir::CoroutineKind::Coroutine(_)) => break 'asyncness hir::IsAsync::NotAsync,
                                hir::ClosureKind::Coroutine(hir::CoroutineKind::Desugared(coroutine_desugaring, _)) => coroutine_desugaring,
                                hir::ClosureKind::CoroutineClosure(coroutine_desugaring) => coroutine_desugaring,
                            };
                            match coroutine_desugaring {
                                hir::CoroutineDesugaring::Async | hir::CoroutineDesugaring::AsyncGen => hir::IsAsync::Async(*decl_span_hir),
                                _ => hir::IsAsync::NotAsync,
                            }
                        },
                        abi: ExternAbi::Rust,
                    },
                    decl: decl_hir,
                    span: *decl_span_hir,
                };
                visitor.visit_fn(kind_ast, coroutine_kind_ast, expr_ast.span, expr_ast.id, kind_hir, generics_hir, sig_hir, Some(*body_hir), expr_hir.span, expr_hir.hir_id);
            }
            (ast::ExprKind::Block(block_ast, _), hir::ExprKind::Block(block_hir, _)) => {
                visitor.visit_block(block_ast, block_hir);
            }
            (ast::ExprKind::Gen(_, block_ast, ast::GenBlockKind::Async, _), hir::ExprKind::Closure(closure_hir)) => {
                let body_hir = visitor.nested_body(closure_hir.body);
                if let Some(body_hir) = body_hir {
                    visit_block_expr(visitor, block_ast, &body_hir.value);
                }
            }
            (ast::ExprKind::Gen(_, block_ast, ast::GenBlockKind::Gen, _), hir::ExprKind::Closure(closure_hir)) => {
                let body_hir = visitor.nested_body(closure_hir.body);
                if let Some(body_hir) = body_hir {
                    visit_block_expr(visitor, block_ast, &body_hir.value);
                }
            }
            (ast::ExprKind::Gen(_, block_ast, ast::GenBlockKind::AsyncGen, _), hir::ExprKind::Closure(closure_hir)) => {
                let body_hir = visitor.nested_body(closure_hir.body);
                if let Some(body_hir) = body_hir {
                    visit_block_expr(visitor, block_ast, &body_hir.value);
                }
            }
            (ast::ExprKind::Await(expr_ast, _), hir::ExprKind::Match(expr_hir, _, hir::MatchSource::AwaitDesugar)) => {
                if let hir::ExprKind::Call(into_future_path, [inner_expr_hir]) = expr_hir.kind
                    && let hir::ExprKind::Path(hir::QPath::LangItem(hir::LangItem::IntoFutureIntoFuture, _)) = into_future_path.kind
                {
                    visit_matching_expr(visitor, expr_ast, inner_expr_hir);
                }
            }
            (ast::ExprKind::TryBlock(_), _) => {
                // TODO
            }
            (ast::ExprKind::Assign(left_ast, right_ast, _), hir::ExprKind::Assign(left_hir, right_hir, _)) => {
                visit_matching_expr(visitor, left_ast, left_hir);
                visit_matching_expr(visitor, right_ast, right_hir);
            }
            (ast::ExprKind::Assign(left_ast, right_ast, _), hir::ExprKind::Block(block_hir, None)) => {
                let mut assigned_subexprs_ast = vec![];
                assigned_subexprs_in_expr_assign_lhs(visitor.def_res(), left_ast, &mut assigned_subexprs_ast);

                if let [destructure_let_hir, assignments_hir @ ..] = block_hir.stmts {
                    for (assigned_subexpr_ast, assignment_hir) in iter::zip(assigned_subexprs_ast, assignments_hir) {
                        let hir::StmtKind::Expr(assignment_hir) = assignment_hir.kind else { unreachable!() };
                        let hir::ExprKind::Assign(assigned_subexpr_hir, _, _) = assignment_hir.kind else { unreachable!() };
                        visit_matching_expr(visitor, assigned_subexpr_ast, assigned_subexpr_hir);
                    }
                    let hir::StmtKind::Let(destructure_let_hir) = destructure_let_hir.kind else { unreachable!() };
                    let hir::LetStmt { init: Some(right_hir), source: hir::LocalSource::AssignDesugar(_), .. } = destructure_let_hir else { unreachable!() };
                    visit_matching_expr(visitor, right_ast, right_hir);
                }
            }
            (ast::ExprKind::AssignOp(_, left_ast, right_ast), hir::ExprKind::AssignOp(_, left_hir, right_hir)) => {
                visit_matching_expr(visitor, left_ast, left_hir);
                visit_matching_expr(visitor, right_ast, right_hir);
            }
            (ast::ExprKind::Field(expr_ast, _), hir::ExprKind::Field(expr_hir, _)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::Index(expr_ast, index_ast, _), hir::ExprKind::Index(expr_hir, index_hir, _)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                visit_matching_expr(visitor, index_ast, index_hir);
            }
            (ast::ExprKind::Range(Some(start_ast), Some(end_ast), ast::RangeLimits::Closed), hir::ExprKind::Call(path_expr_hir, [start_hir, end_hir])) => {
                if let hir::ExprKind::Path(qpath_hir) = &path_expr_hir.kind
                    && let hir::QPath::LangItem(hir::LangItem::RangeInclusiveNew, _) = qpath_hir
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

                if let hir::QPath::LangItem(lang_item_hir, _) = qpath_hir && *lang_item_hir == expected_lang_item {
                    let mut fields_hir_iter = fields_hir.iter();
                    if let Some(start_ast) = start_ast && let Some(start_field_hir) = fields_hir_iter.next() {
                        visit_matching_expr(visitor, start_ast, start_field_hir.expr);
                    }
                    if let Some(end_ast) = end_ast && let Some(end_field_hir) = fields_hir_iter.next() {
                        visit_matching_expr(visitor, end_ast, end_field_hir.expr);
                    }
                }
            }
            (ast::ExprKind::Path(qself_ast, path_ast), hir::ExprKind::Path(qpath_hir)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
            }
            (ast::ExprKind::AddrOf(_, _, expr_ast), hir::ExprKind::AddrOf(_, _, expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::OffsetOf(ty_ast, _), hir::ExprKind::OffsetOf(ty_hir, _)) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
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
            (ast::ExprKind::Struct(struct_ast), hir::ExprKind::Struct(qpath_hir, fields_hir, tail_expr_hir)) => {
                visitor.visit_qpath(struct_ast.qself.as_deref(), &struct_ast.path, qpath_hir);
                for (field_ast, field_hir) in iter::zip(&struct_ast.fields, *fields_hir) {
                    visitor.visit_expr_field(field_ast, field_hir);
                }
                match (&struct_ast.rest, tail_expr_hir) {
                    (ast::StructRest::None, hir::StructTailExpr::None) => {}
                    (ast::StructRest::Rest(_), hir::StructTailExpr::DefaultFields(_)) => {}
                    (ast::StructRest::Base(base_ast), hir::StructTailExpr::Base(base_hir)) => {
                        visit_matching_expr(visitor, base_ast, base_hir);
                    }
                    _ => unreachable!(),
                }
            }
            (ast::ExprKind::Repeat(expr_ast, anon_const_ast), hir::ExprKind::Repeat(expr_hir, const_arg_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
                visitor.visit_const_arg(anon_const_ast, const_arg_hir);
            }
            (ast::ExprKind::Try(expr_ast), hir::ExprKind::Match(expr_hir, _, hir::MatchSource::TryDesugar(_))) => {
                if let hir::ExprKind::Call(path_expr_hir, [expr_hir]) = &expr_hir.kind
                    && let hir::ExprKind::Path(qpath_hir) = &path_expr_hir.kind
                    && let hir::QPath::LangItem(lang_item_hir, _) = qpath_hir && *lang_item_hir == hir::LangItem::TryTraitBranch
                {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::Yield(yield_kind_ast), hir::ExprKind::Yield(expr_hir, hir::YieldSource::Yield)) => {
                if let Some(expr_ast) = yield_kind_ast.expr() {
                    visit_matching_expr(visitor, expr_ast, expr_hir);
                }
            }
            (ast::ExprKind::Yeet(_), _) => {
                // TODO
            }
            (ast::ExprKind::Become(expr_ast), hir::ExprKind::Become(expr_hir)) => {
                visit_matching_expr(visitor, expr_ast, expr_hir);
            }
            (ast::ExprKind::IncludedBytes(_), _) => {
                // TODO
            }
            // NOTE: By default, rustc performs flattening on `format_args` during lowering to the HIR,
            //       which prevents us from matching up the corresponding `format_args` arguments
            //       between the AST and the HIR.
            //       To use this, `rustc_session::config::UnstableOptions::flatten_format_args`
            //       must be set to `false` in `config.opts.unstable_opts.flatten_format_args`.
            (ast::ExprKind::FormatArgs(format_args_ast), hir::ExprKind::Call(_, args_hir)) => {
                if let [_, format_args_expr_hir, ..] = args_hir {
                    let hir::ExprKind::AddrOf(_, _, format_args_expr_hir) = format_args_expr_hir.kind else { unreachable!() };

                    match format_args_expr_hir.kind {
                        hir::ExprKind::Match(format_args_match_expr_hir, _, hir::MatchSource::FormatArgs) => {
                            let hir::ExprKind::Tup(format_arg_exprs_hir) = format_args_match_expr_hir.kind else { unreachable!() };
                            for (format_arg_ast, format_arg_expr_hir) in iter::zip(format_args_ast.arguments.all_args(), format_arg_exprs_hir) {
                                let hir::ExprKind::AddrOf(_, _, format_arg_expr_hir) = format_arg_expr_hir.kind else { unreachable!() };
                                visit_matching_expr(visitor, &format_arg_ast.expr, format_arg_expr_hir);
                            }
                        }
                        hir::ExprKind::Array(format_arg_format_exprs_hir) => {
                            for (format_arg_ast, format_arg_format_expr_hir) in iter::zip(format_args_ast.arguments.all_args(), format_arg_format_exprs_hir) {
                                let hir::ExprKind::Call(_, [format_arg_expr_hir]) = format_arg_format_expr_hir.kind else { unreachable!() };
                                let hir::ExprKind::AddrOf(_, _, format_arg_expr_hir) = format_arg_expr_hir.kind else { unreachable!() };
                                visit_matching_expr(visitor, &format_arg_ast.expr, format_arg_expr_hir);
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }

            (ast_kind, hir_kind) => {
                let mut diagnostic = visitor.tcx().dcx().struct_warn("unrecognized AST-HIR node pair");
                diagnostic.span_note(expr_ast.span, format!("AST node: {}", ast_kind.descr()));
                diagnostic.span_note(expr_hir.span, format!("HIR node: {}", hir_kind.descr()));
                diagnostic.emit();
            }
        }
    }

    pub fn walk_expr_field<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, expr_field_ast: &'ast ast::ExprField, expr_field_hir: &'hir hir::ExprField<'hir>) {
        visit_matching_expr(visitor, &expr_field_ast.expr, expr_field_hir.expr);
    }

    pub fn walk_arm<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, arm_ast: &'ast ast::Arm, arm_hir: &'hir hir::Arm<'hir>) {
        visit_matching_pat(visitor, &arm_ast.pat, arm_hir.pat);
        if let Some(arm_guard_ast) = &arm_ast.guard && let Some(arm_guard_hir) = arm_hir.guard {
            visit_matching_expr(visitor, arm_guard_ast, arm_guard_hir);
        }
        if let Some(arm_body_ast) = &arm_ast.body {
            visit_matching_expr(visitor, arm_body_ast, arm_hir.body);
        }
    }

    pub fn visit_matching_pat<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, pat_ast: &'ast ast::Pat, pat_hir: &'hir hir::Pat<'hir>) {
        match (&pat_ast.kind, &pat_hir.kind) {
            (ast::PatKind::Paren(pat_ast), _) => {
                visit_matching_pat(visitor, pat_ast, pat_hir);
            }
            (ast::PatKind::Rest, _) => {}
            (ast::PatKind::MacCall(..), _) => {}
            (ast::PatKind::Err(_), _) | (_, hir::PatKind::Err(_)) => {}

            _ => visitor.visit_pat(pat_ast, pat_hir),
        }
    }

    pub fn walk_pat<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, pat_ast: &'ast ast::Pat, pat_hir: &'hir hir::Pat<'hir>) {
        match (&pat_ast.kind, &pat_hir.kind) {
            (ast::PatKind::Paren(pat_ast), _) => {
                visit_matching_pat(visitor, pat_ast, pat_hir);
            }
            (ast::PatKind::Rest, _) => {}
            (ast::PatKind::MacCall(..), _) => {}
            (ast::PatKind::Err(_), _) | (_, hir::PatKind::Err(_)) => {}

            (ast::PatKind::Wild, hir::PatKind::Wild) => {}
            (ast::PatKind::Never, hir::PatKind::Never) => {}
            (ast::PatKind::Ident(_, _ident_ast, pat_ast), hir::PatKind::Binding(_, _, _ident_hir, pat_hir)) => {
                if let Some(pat_ast) = pat_ast && let Some(pat_hir) = pat_hir {
                    visit_matching_pat(visitor, pat_ast, pat_hir);
                }
            }
            (ast::PatKind::Ident(_, _ident_ast, None), hir::PatKind::Expr(_pat_expr_hir)) => {
                // TODO: Visit path
            }
            (ast::PatKind::Path(qself_ast, path_ast), hir::PatKind::Expr(pat_expr_hir)) => {
                if let hir::PatExprKind::Path(qpath_hir) = &pat_expr_hir.kind {
                    visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
                }
            }
            (ast::PatKind::Expr(expr_ast), hir::PatKind::Expr(pat_expr_hir)) => {
                visitor.visit_pat_expr(expr_ast, pat_expr_hir);
            }
            (ast::PatKind::Tuple(pats_ast), hir::PatKind::Tuple(pats_hir, _)) => {
                let pats_ast = pats_ast.iter().filter(|pat_ast| !pat_ast.is_rest());
                for (pat_ast, pat_hir) in iter::zip(pats_ast, *pats_hir) {
                    visit_matching_pat(visitor, pat_ast, pat_hir);
                }
            }
            (ast::PatKind::Struct(qself_ast, path_ast, pat_fields_ast, _), hir::PatKind::Struct(qpath_hir, pat_fields_hir, _)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
                for (pat_field_ast, pat_field_hir) in iter::zip(pat_fields_ast, *pat_fields_hir) {
                    visitor.visit_pat_field(pat_field_ast, pat_field_hir);
                }
            }
            (ast::PatKind::TupleStruct(qself_ast, path_ast, pats_ast), hir::PatKind::TupleStruct(qpath_hir, pats_hir, _)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
                let pats_ast = pats_ast.iter().filter(|pat_ast| !pat_ast.is_rest());
                for (pat_ast, pat_hir) in iter::zip(pats_ast, *pats_hir) {
                    visit_matching_pat(visitor, pat_ast, pat_hir);
                }
            }
            (ast::PatKind::Box(pat_ast), hir::PatKind::Box(pat_hir)) => {
                visit_matching_pat(visitor, pat_ast, pat_hir);
            }
            (ast::PatKind::Ref(pat_ast, _), hir::PatKind::Ref(pat_hir, _)) => {
                visit_matching_pat(visitor, pat_ast, pat_hir);
            }
            (ast::PatKind::Deref(pat_ast), hir::PatKind::Deref(pat_hir)) => {
                visit_matching_pat(visitor, pat_ast, pat_hir);
            }
            (ast::PatKind::Or(pats_ast), hir::PatKind::Or(pats_hir)) => {
                for (pat_ast, pat_hir) in iter::zip(pats_ast, *pats_hir) {
                    visit_matching_pat(visitor, pat_ast, pat_hir);
                }
            }
            (ast::PatKind::Range(start_expr_ast, end_expr_ast, _), hir::PatKind::Range(start_expr_hir, end_expr_hir, _)) => {
                if let Some(start_expr_ast) = start_expr_ast && let Some(start_expr_hir) = start_expr_hir {
                    visitor.visit_pat_expr(start_expr_ast, start_expr_hir);
                }
                if let Some(end_expr_ast) = end_expr_ast && let Some(end_expr_hir) = end_expr_hir {
                    visitor.visit_pat_expr(end_expr_ast, end_expr_hir);
                }
            }
            (ast::PatKind::Slice(pats_ast), hir::PatKind::Slice(pats_before_hir, slice_hir, pats_after_hir)) => {
                for (pat_ast, pat_hir) in iter::zip(&pats_ast[..pats_before_hir.len()], *pats_before_hir) {
                    visit_matching_pat(visitor, pat_ast, pat_hir);
                }
                if let Some(slice_hir) = slice_hir {
                    visit_matching_pat(visitor, &pats_ast[pats_before_hir.len()], slice_hir);
                }
                if !pats_after_hir.is_empty() {
                    for (pat_ast, pat_hir) in iter::zip(&pats_ast[(pats_before_hir.len() + 1)..], *pats_after_hir) {
                        visit_matching_pat(visitor, pat_ast, pat_hir);
                    }
                }
            }

            (ast_kind, hir_kind) => {
                let mut diagnostic = visitor.tcx().dcx().struct_warn("unrecognized AST-HIR node pair");
                diagnostic.span_note(pat_ast.span, format!("AST node: {}", ast_kind.descr()));
                diagnostic.span_note(pat_hir.span, format!("HIR node: {}", hir_kind.descr()));
                diagnostic.emit();
            }
        }
    }

    pub fn walk_pat_expr<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, pat_expr_ast: &'ast ast::Expr, pat_expr_hir: &'hir hir::PatExpr<'hir>) {
        match (&pat_expr_ast.kind, &pat_expr_hir.kind) {
            (ast::ExprKind::Lit(_lit_ast), hir::PatExprKind::Lit { lit: _lit_hir, negated: false }) => {}
            (ast::ExprKind::Unary(ast::UnOp::Neg, inner_expr_ast), hir::PatExprKind::Lit { lit: _lit_hir, negated: true }) if let ast::ExprKind::Lit(_lit_ast) = &inner_expr_ast.kind => {}
            (ast::ExprKind::IncludedBytes(_bytes_ast), hir::PatExprKind::Lit { lit: _lit_hir, negated: false }) => {}
            (ast::ExprKind::Path(qself_ast, path_ast), hir::PatExprKind::Path(qpath_hir)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
            }
            (ast::ExprKind::ConstBlock(_const_block_ast), hir::PatExprKind::ConstBlock(_const_block_hir)) => {
                // TODO: Visit const block.
            }
            _ => unreachable!(),
        }
    }

    pub fn walk_pat_field<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, pat_field_ast: &'ast ast::PatField, pat_field_hir: &'hir hir::PatField<'hir>) {
        visit_matching_pat(visitor, &pat_field_ast.pat, pat_field_hir.pat);
    }

    pub fn visit_matching_ty<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, ty_ast: &'ast ast::Ty, ty_hir: &'hir hir::Ty<'hir>) {
        match (&ty_ast.kind, &ty_hir.kind) {
            (ast::TyKind::Paren(ty_ast), _) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
            }
            (ast::TyKind::MacCall(..), _) => {}
            (ast::TyKind::Dummy, _) => {}
            (ast::TyKind::Err(_), _) | (_, hir::TyKind::Err(_)) => {}

            _ => visitor.visit_ty(ty_ast, ty_hir),
        }
    }

    pub fn walk_ty<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, ty_ast: &'ast ast::Ty, ty_hir: &'hir hir::Ty<'hir>) {
        match (&ty_ast.kind, &ty_hir.kind) {
            (ast::TyKind::Paren(ty_ast), _) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
            }
            (ast::TyKind::MacCall(..), _) => {}
            (ast::TyKind::Dummy, _) => {}
            (ast::TyKind::Err(_), _) | (_, hir::TyKind::Err(_)) => {}

            (ast::TyKind::Never, hir::TyKind::Never) => {}
            (ast::TyKind::Path(qself_ast, path_ast), hir::TyKind::Path(qpath_hir)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
            }
            (ast::TyKind::Ptr(mut_ty_ast), hir::TyKind::Ptr(mut_ty_hir)) => {
                visit_matching_ty(visitor, &mut_ty_ast.ty, mut_ty_hir.ty);
            }
            (ast::TyKind::Ref(_lifetime_ast, mut_ty_ast), hir::TyKind::Ref(_lifetime_hir, mut_ty_hir)) => {
                visit_matching_ty(visitor, &mut_ty_ast.ty, mut_ty_hir.ty);
            }
            (ast::TyKind::Slice(ty_ast), hir::TyKind::Slice(ty_hir)) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
            }
            (ast::TyKind::Array(ty_ast, anon_const_ast), hir::TyKind::Array(ty_hir, const_arg_hir)) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
                visitor.visit_const_arg(anon_const_ast, const_arg_hir);
            }
            (ast::TyKind::Tup(tys_ast), hir::TyKind::Tup(tys_hir)) => {
                for (ty_ast, ty_hir) in iter::zip(tys_ast, *tys_hir) {
                    visit_matching_ty(visitor, ty_ast, ty_hir);
                }
            }
            (ast::TyKind::BareFn(bare_fn_ty_ast), hir::TyKind::BareFn(bare_fn_ty_hir)) => {
                for (param_ast, (_param_ident_hir, param_hir_ty)) in iter::zip(&bare_fn_ty_ast.decl.inputs, iter::zip(bare_fn_ty_hir.param_idents, bare_fn_ty_hir.decl.inputs)) {
                    visit_matching_ty(visitor, &param_ast.ty, param_hir_ty);
                }
                match (&bare_fn_ty_ast.decl.output, bare_fn_ty_hir.decl.output) {
                    (ast::FnRetTy::Default(_), hir::FnRetTy::DefaultReturn(_)) => {}
                    (ast::FnRetTy::Ty(ret_ty_ast), hir::FnRetTy::Return(ret_ty_hir)) => {
                        visit_matching_ty(visitor, ret_ty_ast, ret_ty_hir);
                    }
                    _ => unreachable!(),
                }
                for (generic_param_ast, generic_param_hir) in iter::zip(&bare_fn_ty_ast.generic_params, bare_fn_ty_hir.generic_params) {
                    visitor.visit_generic_param(generic_param_ast, generic_param_hir);
                }
            }
            (ast::TyKind::TraitObject(generic_bounds_ast, _), hir::TyKind::TraitObject(poly_trait_refs_hir, _)) => {
                let poly_trait_refs_ast = generic_bounds_ast.iter().filter_map(|generic_bound_ast| {
                    match generic_bound_ast {
                        ast::GenericBound::Trait(poly_trait_ref_ast) => Some(poly_trait_ref_ast),
                        _ => None,
                    }
                });
                for (poly_trait_ref_ast, poly_trait_ref_hir) in iter::zip(poly_trait_refs_ast, *poly_trait_refs_hir) {
                    visitor.visit_poly_trait_ref(poly_trait_ref_ast, poly_trait_ref_hir);
                }
                // TODO: Visit lifetime
            }
            (ast::TyKind::ImplTrait(node_id, generic_bounds_ast), hir::TyKind::OpaqueDef(opaque_ty_hir)) => {
                visitor.visit_impl_trait(*node_id, generic_bounds_ast, opaque_ty_hir);
            }
            // NOTE: `impl Trait` in param type positions are represented as paths to a synthetic generic type parameter on the owner node.
            //       The owner node also has the generic bounds corresponding to the `impl Trait` as part of a generated generic predicate.
            (ast::TyKind::ImplTrait(node_id, generic_bounds_ast), hir::TyKind::Path(_qpath_hir)) => {
                let Some((def_id, _)) = ty_hir.as_generic_param() else { unreachable!() };

                let owner_node = visitor.tcx().hir_owner_node(ty_hir.hir_id.owner);
                let Some(owner_generics) = owner_node.generics() else { unreachable!() };

                let Some((impl_trait_predicate_hir_id, impl_trait_predicate_hir)) = owner_generics.predicates.iter().find_map(|where_predicate| {
                    let hir::WherePredicateKind::BoundPredicate(predicate_hir) = where_predicate.kind else { return None; };
                    if predicate_hir.origin != hir::PredicateOrigin::ImplTrait {return None; }
                    predicate_hir.is_param_bound(def_id).then_some((where_predicate.hir_id, predicate_hir))
                }) else { unreachable!() };

                visitor.visit_impl_trait_in_param(*node_id, generic_bounds_ast, impl_trait_predicate_hir_id, impl_trait_predicate_hir);
            }
            (ast::TyKind::Typeof(anon_const_ast), hir::TyKind::Typeof(anon_const_hir)) => {
                visitor.visit_anon_const(anon_const_ast, anon_const_hir);
            }
            (ast::TyKind::ImplicitSelf, hir::TyKind::Path(_qpath_hir)) => {
                // TODO: Visit path
            }
            (ast::TyKind::Infer, hir::TyKind::Infer(_)) => {}
            (ast::TyKind::CVarArgs, _) => {}
            (ast::TyKind::Pat(ty_ast, ty_pat_ast), hir::TyKind::Pat(ty_hir, ty_pat_hir)) => {
                visit_matching_ty(visitor, ty_ast, ty_hir);
                visitor.visit_ty_pat(ty_pat_ast, ty_pat_hir);
            }

            (ast_kind, hir_kind) => {
                let mut diagnostic = visitor.tcx().dcx().struct_warn("unrecognized AST-HIR node pair");
                diagnostic.span_note(ty_ast.span, format!("AST node: {}", ast_kind.descr()));
                diagnostic.span_note(ty_hir.span, format!("HIR node: {}", hir_kind.descr()));
                diagnostic.emit();
            }
        }
    }

    pub fn walk_ty_pat<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, ty_pat_ast: &'ast ast::TyPat, ty_pat_hir: &'hir hir::TyPat<'hir>) {
        match (&ty_pat_ast.kind, &ty_pat_hir.kind) {
            (ast::TyPatKind::Err(_), hir::TyPatKind::Err(_)) => {}

            (ast::TyPatKind::Or(ty_pats_ast), hir::TyPatKind::Or(ty_pats_hir)) => {
                for (ty_pat_ast, ty_pat_hir) in iter::zip(ty_pats_ast, *ty_pats_hir) {
                    visitor.visit_ty_pat(ty_pat_ast, ty_pat_hir);
                }
            }
            (ast::TyPatKind::Range(start_anon_const_ast, end_anon_const_ast, _), hir::TyPatKind::Range(start_const_arg_hir, end_const_arg_hir)) => {
                if let Some(start_anon_const_ast) = start_anon_const_ast {
                    visitor.visit_const_arg(start_anon_const_ast, start_const_arg_hir);
                }
                if let Some(end_anon_const_ast) = end_anon_const_ast {
                    visitor.visit_const_arg(end_anon_const_ast, end_const_arg_hir);
                }
            }

            _ => unreachable!(),
        }
    }

    pub fn walk_impl_trait<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, _node_id: ast::NodeId, generic_bounds_ast: &'ast ast::GenericBounds, opaque_ty_hir: &'hir hir::OpaqueTy<'hir>) {
        for (generic_bound_ast, generic_bound_hir) in iter::zip(generic_bounds_ast, opaque_ty_hir.bounds) {
            visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
        }
    }

    pub fn walk_impl_trait_in_param<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, _node_id: ast::NodeId, generic_bounds_ast: &'ast ast::GenericBounds, _impl_trait_predicate_hir_id: hir::HirId, impl_trait_predicate_hir: &'hir hir::WhereBoundPredicate<'hir>) {
        for (generic_bound_ast, generic_bound_hir) in iter::zip(generic_bounds_ast, impl_trait_predicate_hir.bounds) {
            visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
        }
    }

    pub fn walk_path<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, path_ast: &'ast ast::Path, path_hir: &'hir hir::Path<'hir>) {
        for (path_segment_ast, path_segment_hir) in iter::zip(&path_ast.segments, path_hir.segments) {
            visitor.visit_path_segment(path_segment_ast, path_segment_hir);
        }
    }

    pub fn walk_qpath<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, qself_ast: Option<&'ast ast::QSelf>, path_ast: &'ast ast::Path, qpath_hir: &'hir hir::QPath<'hir>) {
        fn walk_qpath_impl<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, qself_ast: Option<&'ast ast::QSelf>, path_segments_ast: &'ast [ast::PathSegment], qpath_hir: &'hir hir::QPath<'hir>) {
            match qpath_hir {
                hir::QPath::Resolved(ty_hir, path_hir) => {
                    match (qself_ast, ty_hir) {
                        (Some(qself_ast), Some(ty_hir)) => visit_matching_ty(visitor, &qself_ast.ty, ty_hir),
                        (Some(qself_ast), None) if qself_ast.position == 0 => {}
                        (None, None) => {}
                        _ => unreachable!(),
                    }

                    let (offset_ast, offset_hir) = match (path_segments_ast.first().is_some_and(|segment| segment.ident.name == kw::PathRoot), path_hir.is_global()) {
                        (true, true) => (0, 0),
                        (true, false) => (1, 0),
                        (false, true) => (0, 1),
                        (false, false) => (0, 0),
                    };

                    for (path_segment_ast, path_segment_hir) in iter::zip(&path_segments_ast[offset_ast..], &path_hir.segments[offset_hir..]) {
                        visitor.visit_path_segment(path_segment_ast, path_segment_hir);
                    }
                }
                hir::QPath::TypeRelative(ty_hir, path_segment_hir) => {
                    // Visit the qualified self type, if the rest of the beginning of the path
                    // (excluding the last path segment) was all denoted as a qualified self in the AST.
                    if let Some(qself_ast) = qself_ast && qself_ast.position == path_segments_ast.len() - 1 {
                        visit_matching_ty(visitor, &qself_ast.ty, ty_hir);
                    }

                    // Visit every path segment except the last one.
                    if let hir::TyKind::Path(qself_qpath_hir) = &ty_hir.kind {
                        walk_qpath_impl(visitor, qself_ast, &path_segments_ast[0..(path_segments_ast.len() - 1)], qself_qpath_hir);
                    }

                    // Visit the last path segment.
                    let Some(last_path_segment_ast) = path_segments_ast.last() else { unreachable!() };
                    visitor.visit_path_segment(last_path_segment_ast, path_segment_hir);
                }
                hir::QPath::LangItem(_, _) => unreachable!(),
            }
        }

        walk_qpath_impl(visitor, qself_ast, &path_ast.segments, qpath_hir);
    }

    pub fn walk_path_segment<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, path_segment_ast: &'ast ast::PathSegment, path_segment_hir: &'hir hir::PathSegment<'hir>) {
        if let Some(generic_args_ast) = &path_segment_ast.args && let Some(generic_args_hir) = path_segment_hir.args {
            visitor.visit_generic_args(generic_args_ast, generic_args_hir);
        }
    }

    pub fn walk_generic_args<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, generic_args_ast: &'ast ast::GenericArgs, generic_args_hir: &'hir hir::GenericArgs<'hir>) {
        match (generic_args_ast, generic_args_hir.parenthesized) {
            (ast::GenericArgs::AngleBracketed(generic_args_ast), hir::GenericArgsParentheses::No) => {
                let mut args_hir = generic_args_hir.args.iter().filter(|arg_hir| {
                    let hir::GenericArg::Lifetime(lifetime_hir) = arg_hir else { return true; };
                    !lifetime_hir.is_implicit()
                });
                let mut constraints_hir = generic_args_hir.constraints.iter();
                for generic_arg_ast in &generic_args_ast.args {
                    match generic_arg_ast {
                        ast::AngleBracketedArg::Arg(arg_ast) => {
                            let Some(arg_hir) = args_hir.next() else { unreachable!() };
                            visitor.visit_generic_arg(arg_ast, arg_hir);
                        }
                        ast::AngleBracketedArg::Constraint(constraint_ast) => {
                            let Some(constraint_hir) = constraints_hir.next() else { unreachable!() };
                            visitor.visit_assoc_item_constraint(constraint_ast, constraint_hir);
                        }
                    }
                }
            }
            (ast::GenericArgs::Parenthesized(generic_args_ast), hir::GenericArgsParentheses::ParenSugar) => {
                let Some(input_generic_arg_hir) = generic_args_hir.args.first() else { unreachable!() };
                let hir::GenericArg::Type(input_tup_ty_hir) = input_generic_arg_hir else { unreachable!() };
                let hir::TyKind::Tup(input_tys_hir) = input_tup_ty_hir.kind else { unreachable!() };
                for (input_ty_ast, input_ty_hir) in iter::zip(&generic_args_ast.inputs, input_tys_hir) {
                    visit_matching_ty(visitor, input_ty_ast, input_ty_hir);
                }

                let Some(output_ty_constraint_hir) = generic_args_hir.constraints.first() else { unreachable!() };
                let hir::AssocItemConstraintKind::Equality { term: output_ty_constraint_term_hir } = output_ty_constraint_hir.kind else { unreachable!() };
                let hir::Term::Ty(output_ty_hir) = output_ty_constraint_term_hir else { unreachable!() };
                if let ast::FnRetTy::Ty(output_ty_ast) = &generic_args_ast.output {
                    visit_matching_ty(visitor, output_ty_ast, output_ty_hir);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn walk_generic_arg<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, generic_arg_ast: &'ast ast::GenericArg, generic_arg_hir: &'hir hir::GenericArg<'hir>) {
        match (generic_arg_ast, generic_arg_hir) {
            (ast::GenericArg::Lifetime(_lifetime_ast), hir::GenericArg::Lifetime(_lifetime_hir)) => {
                // TODO
            }
            (ast::GenericArg::Type(ty_ast), hir::GenericArg::Type(ty_hir)) => {
                visit_matching_ty(visitor, ty_ast, ty_hir.as_unambig_ty());
            }
            (ast::GenericArg::Const(anon_const_ast), hir::GenericArg::Const(const_arg_hir)) => {
                visitor.visit_const_arg(anon_const_ast, const_arg_hir.as_unambig_ct());
            }
            // NOTE: Paths to named constants are expressed as generic type arguments in the AST.
            (ast::GenericArg::Type(ty_ast), hir::GenericArg::Const(const_arg_hir)) if let ast::TyKind::Path(_, _) = &ty_ast.kind => {
                visitor.visit_path_anon_const(ty_ast, const_arg_hir.as_unambig_ct());
            }
            (ast::GenericArg::Lifetime(_) | ast::GenericArg::Type(_) | ast::GenericArg::Const(_), hir::GenericArg::Infer(_infer_arg_hir)) => {}
            _ => {
                let mut diagnostic = visitor.tcx().dcx().struct_warn("unrecognized AST-HIR node pair");
                diagnostic.span_note(generic_arg_ast.span(), format!("AST node: {}", match generic_arg_ast {
                    ast::GenericArg::Lifetime(_) => "lifetime",
                    ast::GenericArg::Type(_) => "type",
                    ast::GenericArg::Const(_) => "constant",
                }));
                diagnostic.span_note(generic_arg_hir.span(), format!("HIR node: {}", generic_arg_hir.descr()));
                diagnostic.emit();
            }
        }
    }

    pub fn walk_assoc_item_constraint<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, constraint_ast: &'ast ast::AssocItemConstraint, constraint_hir: &'hir hir::AssocItemConstraint<'hir>) {
        if let Some(generic_args_ast) = &constraint_ast.gen_args {
            visitor.visit_generic_args(generic_args_ast, constraint_hir.gen_args);
        }

        match (&constraint_ast.kind, constraint_hir.kind) {
            (ast::AssocItemConstraintKind::Equality { term: term_ast }, hir::AssocItemConstraintKind::Equality { term: term_hir }) => {
                match (term_ast, term_hir) {
                    (ast::Term::Ty(ty_ast), hir::Term::Ty(ty_hir)) => {
                        visit_matching_ty(visitor, ty_ast, ty_hir);
                    }
                    (ast::Term::Const(anon_const_ast), hir::Term::Const(anon_const_hir)) => {
                        visitor.visit_const_arg(anon_const_ast, anon_const_hir);
                    }
                    _ => unreachable!(),
                }
            }
            (ast::AssocItemConstraintKind::Bound { bounds: generic_bounds_ast }, hir::AssocItemConstraintKind::Bound { bounds: generic_bounds_hir }) => {
                for (generic_bound_ast, generic_bound_hir) in iter::zip(generic_bounds_ast, generic_bounds_hir) {
                    visitor.visit_generic_bound(generic_bound_ast, generic_bound_hir);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn walk_anon_const<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, anon_const_ast: &'ast ast::AnonConst, anon_const_hir: &'hir hir::AnonConst) {
        let body_hir = visitor.nested_body(anon_const_hir.body);

        if let Some(body_hir) = body_hir {
            visit_matching_expr(visitor, &anon_const_ast.value, &body_hir.value);
        }
    }

    pub fn walk_path_anon_const<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, ty_ast: &'ast ast::Ty, const_arg_hir: &'hir hir::ConstArg<'hir>) {
        match (&ty_ast.kind, &const_arg_hir.kind) {
            (ast::TyKind::Path(qself_ast, path_ast), hir::ConstArgKind::Path(qpath_hir)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
            }
            (ast::TyKind::Path(qself_ast, path_ast), hir::ConstArgKind::Anon(anon_const_hir)) => {
                let body_hir = visitor.nested_body(anon_const_hir.body);

                if let Some(body_hir) = body_hir {
                    if let hir::ExprKind::Path(qpath_hir) = &body_hir.value.kind {
                        visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn walk_const_block<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, anon_const_ast: &'ast ast::AnonConst, const_block_hir: &'hir hir::ConstBlock) {
        let body_hir = visitor.nested_body(const_block_hir.body);

        if let Some(body_hir) = body_hir {
            visit_matching_expr(visitor, &anon_const_ast.value, &body_hir.value);
        }
    }

    pub fn walk_const_arg<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(visitor: &mut T, anon_const_ast: &'ast ast::AnonConst, const_arg_hir: &'hir hir::ConstArg<'hir>) {
        match (&anon_const_ast.value.kind, &const_arg_hir.kind) {
            (ast::ExprKind::Underscore, hir::ConstArgKind::Infer(_, _)) => {
                // TODO
            }
            (ast::ExprKind::Path(qself_ast, path_ast), hir::ConstArgKind::Path(qpath_hir)) => {
                visitor.visit_qpath(qself_ast.as_deref(), path_ast, qpath_hir);
            }
            (_, hir::ConstArgKind::Anon(anon_const_hir)) => {
                // NOTE: We use a `walk_*` method (instead of a `visit_*` method) here intentionally,
                //       as we only want to visit the inner expression.
                //       This is because the anon const in the AST is already paired up with the
                //       const arg HIR container type, and we do not want to amend this mapping.
                walk_anon_const(visitor, anon_const_ast, anon_const_hir);
            }
            _ => unreachable!(),
        }
    }

    pub trait VisitWithHirNode {
        fn visit<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(&'ast self, visitor: &mut T, node_hir: hir::Node<'hir>) -> bool;
    }

    impl VisitWithHirNode for ast::Item {
        fn visit<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(&'ast self, visitor: &mut T, node_hir: hir::Node<'hir>) -> bool {
            let tcx = visitor.tcx();

            match &self.kind {
                ast::ItemKind::Fn(_) => {
                    let Some(fn_ast) = ast::FnItem::from_item(self) else { unreachable!() };
                    let Some(fn_hir) = hir::FnItem::from_node(tcx, node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_fn_item(visitor, &fn_ast, &fn_hir);
                }
                ast::ItemKind::Const(const_ast) => {
                    let Some(const_hir) = hir::ConstItem::from_node(tcx, node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_const(visitor, &const_ast, &const_hir);
                }
                ast::ItemKind::Static(static_ast) => {
                    let Some(static_hir) = hir::StaticItem::from_node(tcx, node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_static(visitor, &static_ast, &static_hir);
                }
                ast::ItemKind::Enum(_, generics_ast, enum_def_ast) => {
                    let hir::Node::Item(item_hir) = node_hir else { panic!("mismatched HIR node") };
                    let hir::ItemKind::Enum(_, generics_hir, enum_def_hir) = &item_hir.kind else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_enum(visitor, enum_def_ast, generics_ast, enum_def_hir, generics_hir);
                }
                ast::ItemKind::Struct(_, generics_ast, variant_data_ast) | ast::ItemKind::Union(_, generics_ast, variant_data_ast) => {
                    let hir::Node::Item(item_hir) = node_hir else { panic!("mismatched HIR node") };
                    let (hir::ItemKind::Struct(_, generics_hir, variant_data_hir) | hir::ItemKind::Union(_, generics_hir, variant_data_hir)) = &item_hir.kind else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_struct(visitor, variant_data_ast, generics_ast, variant_data_hir, generics_hir);
                }
                ast::ItemKind::TyAlias(ty_alias_ast) => {
                    let Some(ty_alias_hir) = hir::TyAliasItem::from_node(node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_ty_alias(visitor, &ty_alias_ast, &ty_alias_hir);
                }
                ast::ItemKind::Trait(trait_ast) => {
                    let hir::Node::Item(item_hir) = node_hir else { panic!("mismatched HIR node") };
                    let hir::ItemKind::Trait(_, _, _, generics_hir, generic_bounds_hir, _) = &item_hir.kind else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_trait(visitor, trait_ast, generics_hir, generic_bounds_hir);
                }
                ast::ItemKind::TraitAlias(_, generics_ast, generic_bounds_ast) => {
                    let hir::Node::Item(item_hir) = node_hir else { panic!("mismatched HIR node") };
                    let hir::ItemKind::TraitAlias(_, generics_hir, generic_bounds_hir) = &item_hir.kind else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_trait_alias(visitor, generics_ast, generic_bounds_ast, generics_hir, generic_bounds_hir);
                }
                ast::ItemKind::Impl(impl_ast) => {
                    let hir::Node::Item(item_hir) = node_hir else { panic!("mismatched HIR node") };
                    let hir::ItemKind::Impl(impl_hir) = &item_hir.kind else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_impl(visitor, impl_ast, impl_hir);
                }
                _ => { return false; }
            }

            true
        }
    }

    impl VisitWithHirNode for ast::AssocItem {
        fn visit<'ast, 'hir, T: AstHirVisitor<'ast, 'hir>>(&'ast self, visitor: &mut T, node_hir: hir::Node<'hir>) -> bool {
            let tcx = visitor.tcx();

            match &self.kind {
                ast::AssocItemKind::Fn(_) => {
                    let Some(fn_ast) = ast::FnItem::from_assoc_item(self) else { unreachable!() };
                    let Some(fn_hir) = hir::FnItem::from_node(tcx, node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_fn_item(visitor, &fn_ast, &fn_hir);
                }
                ast::AssocItemKind::Const(const_ast) => {
                    let Some(const_hir) = hir::ConstItem::from_node(tcx, node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_const(visitor, &const_ast, &const_hir);
                }
                ast::AssocItemKind::Type(ty_alias_ast) => {
                    let Some(ty_alias_hir) = hir::TyAliasItem::from_node(node_hir) else { panic!("mismatched HIR node") };
                    AstHirVisitor::visit_ty_alias(visitor, &ty_alias_ast, &ty_alias_hir)
                }
                _ => { return false; }
            }

            true
        }
    }
}

pub struct BodyResolutions<'tcx> {
    tcx: TyCtxt<'tcx>,
    node_id_to_hir_id: FxHashMap<ast::NodeId, hir::HirId>,
    hir_id_to_node_id: FxHashMap<hir::HirId, ast::NodeId>,
}

impl<'tcx> BodyResolutions<'tcx> {
    pub(crate) fn empty(tcx: TyCtxt<'tcx>) -> Self {
        Self { tcx, node_id_to_hir_id: Default::default(), hir_id_to_node_id: Default::default() }
    }

    pub fn ast_id(&self, hir_id: hir::HirId) -> Option<ast::NodeId> {
        self.hir_id_to_node_id.get(&hir_id).copied()
    }

    pub fn hir_id(&self, node_id: ast::NodeId) -> Option<hir::HirId> {
        self.node_id_to_hir_id.get(&node_id).copied()
    }

    pub fn hir_node(&self, node_id: ast::NodeId) -> Option<hir::Node<'tcx>> {
        self.node_id_to_hir_id.get(&node_id).map(|&hir_id| self.tcx.hir_node(hir_id))
    }

    pub fn hir_param(&self, param: &ast::Param) -> Option<&'tcx hir::Param<'tcx>> {
        self.hir_node(param.id).map(|hir_node| hir_node.expect_param())
    }

    pub fn hir_pat(&self, pat: &ast::Pat) -> Option<&'tcx hir::Pat<'tcx>> {
        self.hir_node(pat.id).map(|hir_node| hir_node.expect_pat())
    }

    pub fn hir_expr(&self, expr: &ast::Expr) -> Option<&'tcx hir::Expr<'tcx>> {
        self.hir_node(expr.id).map(|hir_node| hir_node.expect_expr())
    }

    pub fn hir_stmt(&self, stmt: &ast::Stmt) -> Option<&'tcx hir::Stmt<'tcx>> {
        self.hir_node(stmt.id).map(|hir_node| hir_node.expect_stmt())
    }

    pub fn hir_block(&self, block: &ast::Block) -> Option<&'tcx hir::Block<'tcx>> {
        self.hir_node(block.id).map(|hir_node| hir_node.expect_block())
    }
}

struct BodyResolutionsCollector<'tcx, 'op> {
    tcx: TyCtxt<'tcx>,
    def_res: &'op DefResolutions,
    node_id_to_hir_id: FxHashMap<ast::NodeId, hir::HirId>,
    hir_id_to_node_id: FxHashMap<hir::HirId, ast::NodeId>,
}

impl<'tcx, 'op> BodyResolutionsCollector<'tcx, 'op> {
    fn new(tcx: TyCtxt<'tcx>, def_res: &'op DefResolutions) -> Self {
        Self {
            tcx,
            def_res,
            node_id_to_hir_id: Default::default(),
            hir_id_to_node_id: Default::default(),
        }
    }

    fn finalize(self) -> BodyResolutions<'tcx> {
        BodyResolutions {
            tcx: self.tcx,
            node_id_to_hir_id: self.node_id_to_hir_id,
            hir_id_to_node_id: self.hir_id_to_node_id,
        }
    }

    fn insert_ids(&mut self, node_id: ast::NodeId, hir_id: hir::HirId) {
        if node_id == ast::DUMMY_NODE_ID || hir_id == hir::HirId::INVALID { return; }

        self.node_id_to_hir_id.insert_same(node_id, hir_id);
        self.hir_id_to_node_id.insert_same(hir_id, node_id);
    }
}

impl<'ast, 'hir, 'op> visit::AstHirVisitor<'ast, 'hir> for BodyResolutionsCollector<'hir, 'op> {
    type NestedFilter = rustc_middle::hir::nested_filter::OnlyBodies;

    fn tcx(&mut self) -> TyCtxt<'hir> {
        self.tcx
    }

    fn def_res(&mut self) -> &DefResolutions {
        self.def_res
    }

    fn visit_fn(&mut self, kind_ast: ast::visit::FnKind<'ast>, coroutine_kind_ast: &'ast Option<ast::CoroutineKind>, span_ast: Span, id_ast: ast::NodeId, kind_hir: hir::intravisit::FnKind<'hir>, generics_hir: Option<&'hir hir::Generics<'hir>>, sig_hir: hir::FnSig<'hir>, body_hir: Option<hir::BodyId>, span_hir: Span, id_hir: hir::HirId) {
        visit::walk_fn(self, kind_ast, coroutine_kind_ast, span_ast, id_ast, kind_hir, generics_hir, sig_hir, body_hir, span_hir, id_hir);
    }

    fn visit_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>, param_ty_hir: &'hir hir::Ty<'hir>) {
        self.insert_ids(param_ast.id, param_hir.hir_id);
        visit::walk_param(self, param_ast, param_hir, param_ty_hir);
    }

    fn visit_coroutine_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>, param_pat_hir: &'hir hir::Pat<'hir>, param_ty_hir: &'hir hir::Ty<'hir>) {
        self.insert_ids(param_ast.id, param_hir.hir_id);
        visit::walk_coroutine_param(self, param_ast, param_hir, param_pat_hir, param_ty_hir);
    }

    fn visit_coroutine_return_impl_trait(&mut self, coroutine_kind_ast: &'ast ast::CoroutineKind, ret_ty_ast: &'ast ast::FnRetTy, ret_ty_hir: &'hir hir::Ty<'hir>) {
        let (node_id, _) = coroutine_kind_ast.return_id();
        self.insert_ids(node_id, ret_ty_hir.hir_id);
        visit::walk_coroutine_return_impl_trait(self, coroutine_kind_ast, ret_ty_ast, ret_ty_hir);
    }

    fn visit_const(&mut self, const_ast: &'ast ast::ConstItem, const_hir: &hir::ConstItem<'hir>) {
        visit::walk_const(self, const_ast, const_hir);
    }

    fn visit_static(&mut self, static_ast: &'ast ast::StaticItem, static_hir: &hir::StaticItem<'hir>) {
        visit::walk_static(self, static_ast, static_hir);
    }

    fn visit_enum(&mut self, enum_def_ast: &'ast ast::EnumDef, generics_ast: &'ast ast::Generics, enum_def_hir: &'hir hir::EnumDef<'hir>, generics_hir: &'hir hir::Generics<'hir>) {
        visit::walk_enum(self, enum_def_ast, generics_ast, enum_def_hir, generics_hir);
    }

    fn visit_struct(&mut self, variant_data_ast: &'ast ast::VariantData, generics_ast: &'ast ast::Generics, variant_data_hir: &'hir hir::VariantData<'hir>, generics_hir: &'hir hir::Generics<'hir>) {
        visit::walk_struct(self, variant_data_ast, generics_ast, variant_data_hir, generics_hir);
    }

    fn visit_variant(&mut self, variant_ast: &'ast ast::Variant, variant_hir: &'hir hir::Variant<'hir>) {
        self.insert_ids(variant_ast.id, variant_hir.hir_id);
        visit::walk_variant(self, variant_ast, variant_hir);
    }

    fn visit_variant_data(&mut self, variant_data_ast: &'ast ast::VariantData, variant_data_hir: &'hir hir::VariantData<'hir>) {
        visit::walk_variant_data(self, variant_data_ast, variant_data_hir);
    }

    fn visit_field_def(&mut self, field_def_ast: &'ast ast::FieldDef, field_def_hir: &'hir hir::FieldDef<'hir>) {
        self.insert_ids(field_def_ast.id, field_def_hir.hir_id);
        visit::walk_field_def(self, field_def_ast, field_def_hir);
    }

    fn visit_ty_alias(&mut self, ty_alias_ast: &'ast ast::TyAlias, ty_alias_hir: &hir::TyAliasItem<'hir>) {
        visit::walk_ty_alias(self, ty_alias_ast, ty_alias_hir);
    }

    fn visit_trait(&mut self, trait_ast: &'ast ast::Trait, generics_hir: &'hir hir::Generics<'hir>, generic_bounds_hir: hir::GenericBounds<'hir>) {
        visit::walk_trait(self, trait_ast, generics_hir, generic_bounds_hir);
    }

    fn visit_trait_alias(&mut self, generics_ast: &'ast ast::Generics, generic_bounds_ast: &'ast ast::GenericBounds, generics_hir: &'hir hir::Generics<'hir>, generic_bounds_hir: hir::GenericBounds<'hir>) {
        visit::walk_trait_alias(self, generics_ast, generic_bounds_ast, generics_hir, generic_bounds_hir);
    }

    fn visit_impl(&mut self, impl_ast: &'ast ast::Impl, impl_hir: &'hir hir::Impl<'hir>) {
        visit::walk_impl(self, impl_ast, impl_hir);
    }

    fn visit_poly_trait_ref(&mut self, poly_trait_ref_ast: &'ast ast::PolyTraitRef, poly_trait_ref_hir: &'hir hir::PolyTraitRef<'hir>) {
        visit::walk_poly_trait_ref(self, poly_trait_ref_ast, poly_trait_ref_hir);
    }

    fn visit_trait_ref(&mut self, trait_ref_ast: &'ast ast::TraitRef, trait_ref_hir: &'hir hir::TraitRef<'hir>) {
        self.insert_ids(trait_ref_ast.ref_id, trait_ref_hir.hir_ref_id);
        visit::walk_trait_ref(self, trait_ref_ast, trait_ref_hir);
    }

    fn visit_generics(&mut self, generics_ast: &'ast ast::Generics, generics_hir: &'hir hir::Generics<'hir>) {
        visit::walk_generics(self, generics_ast, generics_hir);
    }

    fn visit_generic_param(&mut self, generic_param_ast: &'ast ast::GenericParam, generic_param_hir: &'hir hir::GenericParam<'hir>) {
        self.insert_ids(generic_param_ast.id, generic_param_hir.hir_id);
        visit::walk_generic_param(self, generic_param_ast, generic_param_hir);
    }

    fn visit_generic_bound(&mut self, generic_bound_ast: &'ast ast::GenericBound, generic_bound_hir: &'hir hir::GenericBound<'hir>) {
        visit::walk_generic_bound(self, generic_bound_ast, generic_bound_hir);
    }

    fn visit_block(&mut self, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
        self.insert_ids(block_ast.id, block_hir.hir_id);
        visit::walk_block(self, block_ast, block_hir);
    }

    fn visit_stmt(&mut self, stmt_ast: &'ast ast::Stmt, stmt_hir: &'hir hir::Stmt<'hir>) {
        self.insert_ids(stmt_ast.id, stmt_hir.hir_id);
        visit::walk_stmt(self, stmt_ast, stmt_hir);
    }

    fn visit_local(&mut self, local_ast: &'ast ast::Local, local_hir: &'hir hir::LetStmt<'hir>) {
        self.insert_ids(local_ast.id, local_hir.hir_id);
        visit::walk_local(self, local_ast, local_hir);
    }

    fn visit_expr(&mut self, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
        self.insert_ids(expr_ast.id, expr_hir.hir_id);
        visit::walk_expr(self, expr_ast, expr_hir);
    }

    fn visit_expr_field(&mut self, expr_field_ast: &'ast ast::ExprField, expr_field_hir: &'hir hir::ExprField<'hir>) {
        self.insert_ids(expr_field_ast.id, expr_field_hir.hir_id);
        visit::walk_expr_field(self, expr_field_ast, expr_field_hir);
    }

    fn visit_arm(&mut self, arm_ast: &'ast ast::Arm, arm_hir: &'hir hir::Arm<'hir>) {
        self.insert_ids(arm_ast.id, arm_hir.hir_id);
        visit::walk_arm(self, arm_ast, arm_hir);
    }

    fn visit_pat(&mut self, pat_ast: &'ast ast::Pat, pat_hir: &'hir hir::Pat<'hir>) {
        self.insert_ids(pat_ast.id, pat_hir.hir_id);
        visit::walk_pat(self, pat_ast, pat_hir);
    }

    fn visit_pat_expr(&mut self, pat_expr_ast: &'ast ast::Expr, pat_expr_hir: &'hir hir::PatExpr<'hir>) {
        self.insert_ids(pat_expr_ast.id, pat_expr_hir.hir_id);
        visit::walk_pat_expr(self, pat_expr_ast, pat_expr_hir);
    }

    fn visit_pat_field(&mut self, pat_field_ast: &'ast ast::PatField, pat_field_hir: &'hir hir::PatField<'hir>) {
        self.insert_ids(pat_field_ast.id, pat_field_hir.hir_id);
        visit::walk_pat_field(self, pat_field_ast, pat_field_hir);
    }

    fn visit_ty(&mut self, ty_ast: &'ast ast::Ty, ty_hir: &'hir hir::Ty<'hir>) {
        self.insert_ids(ty_ast.id, ty_hir.hir_id);
        visit::walk_ty(self, ty_ast, ty_hir);
    }

    fn visit_ty_pat(&mut self, ty_pat_ast: &'ast ast::TyPat, ty_pat_hir: &'hir hir::TyPat<'hir>) {
        self.insert_ids(ty_pat_ast.id, ty_pat_hir.hir_id);
        visit::walk_ty_pat(self, ty_pat_ast, ty_pat_hir);
    }

    fn visit_impl_trait(&mut self, node_id: ast::NodeId, generic_bounds_ast: &'ast ast::GenericBounds, opaque_ty_hir: &'hir hir::OpaqueTy<'hir>) {
        self.insert_ids(node_id, opaque_ty_hir.hir_id);
        visit::walk_impl_trait(self, node_id, generic_bounds_ast, opaque_ty_hir);
    }

    fn visit_impl_trait_in_param(&mut self, node_id: ast::NodeId, generic_bounds_ast: &'ast ast::GenericBounds, impl_trait_predicate_hir_id: hir::HirId, impl_trait_predicate_hir: &'hir hir::WhereBoundPredicate<'hir>) {
        self.insert_ids(node_id, impl_trait_predicate_hir_id);
        visit::walk_impl_trait_in_param(self, node_id, generic_bounds_ast, impl_trait_predicate_hir_id, impl_trait_predicate_hir);
    }

    fn visit_path(&mut self, path_ast: &'ast ast::Path, path_hir: &'hir hir::Path<'hir>) {
        visit::walk_path(self, path_ast, path_hir);
    }

    fn visit_qpath(&mut self, qself_ast: Option<&'ast ast::QSelf>, path_ast: &'ast ast::Path, qpath_hir: &'hir hir::QPath<'hir>) {
        visit::walk_qpath(self, qself_ast, path_ast, qpath_hir);
    }

    fn visit_path_segment(&mut self, path_segment_ast: &'ast ast::PathSegment, path_segment_hir: &'hir hir::PathSegment<'hir>) {
        self.insert_ids(path_segment_ast.id, path_segment_hir.hir_id);
        visit::walk_path_segment(self, path_segment_ast, path_segment_hir);
    }

    fn visit_generic_args(&mut self, generic_args_ast: &'ast ast::GenericArgs, generic_args_hir: &'hir hir::GenericArgs<'hir>) {
        visit::walk_generic_args(self, generic_args_ast, generic_args_hir);
    }

    fn visit_generic_arg(&mut self, generic_arg_ast: &'ast ast::GenericArg, generic_arg_hir: &'hir hir::GenericArg<'hir>) {
        visit::walk_generic_arg(self, generic_arg_ast, generic_arg_hir);
    }

    fn visit_assoc_item_constraint(&mut self, constraint_ast: &'ast ast::AssocItemConstraint, constraint_hir: &'hir hir::AssocItemConstraint<'hir>) {
        self.insert_ids(constraint_ast.id, constraint_hir.hir_id);
        visit::walk_assoc_item_constraint(self, constraint_ast, constraint_hir);
    }

    fn visit_anon_const(&mut self, anon_const_ast: &'ast ast::AnonConst, anon_const_hir: &'hir hir::AnonConst) {
        self.insert_ids(anon_const_ast.id, anon_const_hir.hir_id);
        visit::walk_anon_const(self, anon_const_ast, anon_const_hir);
    }

    fn visit_path_anon_const(&mut self, ty_ast: &'ast ast::Ty, const_arg_hir: &'hir hir::ConstArg<'hir>) {
        self.insert_ids(ty_ast.id, const_arg_hir.hir_id);
        visit::walk_path_anon_const(self, ty_ast, const_arg_hir);
    }

    fn visit_const_block(&mut self, anon_const_ast: &'ast ast::AnonConst, const_block_hir: &'hir hir::ConstBlock) {
        self.insert_ids(anon_const_ast.id, const_block_hir.hir_id);
        visit::walk_const_block(self, anon_const_ast, const_block_hir);
    }

    fn visit_const_arg(&mut self, anon_const_ast: &'ast ast::AnonConst, const_arg_hir: &'hir hir::ConstArg<'hir>) {
        self.insert_ids(anon_const_ast.id, const_arg_hir.hir_id);
        visit::walk_const_arg(self, anon_const_ast, const_arg_hir);
    }
}

pub fn resolve_body<'tcx, K>(tcx: TyCtxt<'tcx>, def_res: &DefResolutions, item_ast: &ast::Item<K>, node_hir: hir::Node<'tcx>) -> Option<BodyResolutions<'tcx>>
where
    ast::Item<K>: visit::VisitWithHirNode,
{
    let mut collector = BodyResolutionsCollector::new(tcx, def_res);

    let visited = visit::VisitWithHirNode::visit(item_ast, &mut collector, node_hir);
    if !visited { return None; }

    Some(collector.finalize())
}

pub fn resolve_fn_body<'tcx>(tcx: TyCtxt<'tcx>, def_res: &DefResolutions, fn_ast: &ast::FnItem, fn_hir: &hir::FnItem<'tcx>) -> BodyResolutions<'tcx> {
    let mut collector = BodyResolutionsCollector::new(tcx, def_res);
    visit::AstHirVisitor::visit_fn_item(&mut collector, fn_ast, fn_hir);
    collector.finalize()
}

struct BodyMetaVisitor<T> {
    visitor: T,
}

impl<'ast, 'hir, T: visit::AstHirVisitor<'ast, 'hir>> ast::visit::Visitor<'ast> for BodyMetaVisitor<T> {
    fn visit_item(&mut self, item: &'ast ast::Item) {
        let Some(&def_id) = self.visitor.def_res().node_id_to_def_id.get(&item.id) else { return; };
        let node_hir = self.visitor.tcx().hir_node_by_def_id(def_id);
        visit::VisitWithHirNode::visit(item, &mut self.visitor, node_hir);

        ast::visit::walk_item(self, item);
    }

    fn visit_assoc_item(&mut self, assoc_item: &'ast ast::AssocItem, assoc_ctxt: ast::visit::AssocCtxt) {
        let Some(&def_id) = self.visitor.def_res().node_id_to_def_id.get(&assoc_item.id) else { return; };
        let node_hir = self.visitor.tcx().hir_node_by_def_id(def_id);
        visit::VisitWithHirNode::visit(assoc_item, &mut self.visitor, node_hir);

        ast::visit::walk_assoc_item(self, assoc_item, assoc_ctxt);
    }
}

pub fn resolve_bodies<'tcx>(tcx: TyCtxt<'tcx>, def_res: &DefResolutions, krate_ast: &ast::Crate) -> BodyResolutions<'tcx> {
    let mut body_visitor = BodyMetaVisitor { visitor: BodyResolutionsCollector::new(tcx, def_res) };
    ast::visit::Visitor::visit_crate(&mut body_visitor, krate_ast);
    body_visitor.visitor.finalize()
}

struct BodyResValidator<'tcx, 'op> {
    def_res: &'op DefResolutions,
    body_res: &'op BodyResolutions<'tcx>,
}

impl<'tcx, 'op> BodyResValidator<'tcx, 'op> {
    fn check_node_id(&self, node_descr: &str, node_id: ast::NodeId, span: Span) {
        if node_id == ast::DUMMY_NODE_ID || span == DUMMY_SP { return; }

        let tcx = self.body_res.tcx;

        let Some(_hir_id) = self.body_res.hir_id(node_id) else {
            let mut diagnostic = tcx.dcx().struct_warn(format!("invalid AST-HIR mapping for {node_descr}"));
            diagnostic.span(span);
            diagnostic.span_label(span, "no matching HIR node found");
            diagnostic.emit();

            return;
        };
    }
}

impl<'ast, 'tcx, 'op> ast::visit::Visitor<'ast> for BodyResValidator<'tcx, 'op> {
    fn visit_use_tree(&mut self, _use_tree: &'ast ast::UseTree, _id: ast::NodeId, _nested: bool) {
        // Ignore use trees.
    }

    fn visit_attribute(&mut self, _attr: &'ast ast::Attribute) {
        // Ignore attributes.
    }

    fn visit_vis(&mut self, _vis: &'ast ast::Visibility) {
        // Ignore visibilities, as they are not represented directly in the HIR.
    }

    fn visit_fn(&mut self, fn_kind: ast::visit::FnKind<'ast>, _span: Span, _id: ast::NodeId) {
        match fn_kind {
            ast::visit::FnKind::Fn(_fn_ctxt, _vis, fn_data) => {
                self.visit_fn_header(&fn_data.sig.header);
                self.visit_generics(&fn_data.generics);
                match &fn_data.body {
                    Some(_) => {
                        for param in &fn_data.sig.decl.inputs {
                            self.visit_param(param);
                        }
                    }
                    None => {
                        for param in &fn_data.sig.decl.inputs {
                            self.visit_ty(&param.ty);
                        }
                    }
                }
                self.visit_fn_ret_ty(&fn_data.sig.decl.output);
                if let Some(body) = &fn_data.body { self.visit_block(body); }
            }
            ast::visit::FnKind::Closure(closure_binder, _coroutine_kind, fn_decl, body) => {
                self.visit_closure_binder(closure_binder);
                for param in &fn_decl.inputs {
                    self.visit_param(param);
                }
                self.visit_fn_ret_ty(&fn_decl.output);
                self.visit_expr(body);
            }
        }
    }

    fn visit_param(&mut self, param: &'ast ast::Param) {
        self.check_node_id("parameter", param.id, param.span);
        ast::visit::walk_param(self, param)
    }

    fn visit_trait_ref(&mut self, trait_ref: &'ast ast::TraitRef) {
        self.check_node_id("trait reference", trait_ref.ref_id, trait_ref.path.span);
        ast::visit::walk_trait_ref(self, trait_ref)
    }

    fn visit_generic_param(&mut self, generic_param: &'ast ast::GenericParam) {
        self.check_node_id("generic parameter", generic_param.id, generic_param.ident.span);
        ast::visit::walk_generic_param(self, generic_param)
    }

    fn visit_precise_capturing_arg(&mut self, arg: &'ast ast::PreciseCapturingArg) {
        match arg {
            ast::PreciseCapturingArg::Arg(path, id) => {
                self.check_node_id("precise capturing arg", *id, path.span);
            }
            _ => {}
        }
        ast::visit::walk_precise_capturing_arg(self, arg)
    }

    fn visit_variant(&mut self, variant: &'ast ast::Variant) {
        self.check_node_id("variant", variant.id, variant.span);
        ast::visit::walk_variant(self, variant)
    }

    fn visit_field_def(&mut self, field_def: &'ast ast::FieldDef) {
        self.check_node_id("field definition", field_def.id, field_def.span);
        ast::visit::walk_field_def(self, field_def)
    }

    fn visit_lifetime(&mut self, lifetime: &'ast ast::Lifetime, _ctxt: ast::visit::LifetimeCtxt) {
        // TODO: Check node id.
        ast::visit::walk_lifetime(self, lifetime)
    }

    fn visit_anon_const(&mut self, anon_const: &'ast ast::AnonConst) {
        self.check_node_id("anonymous const", anon_const.id, anon_const.value.span);

        match &anon_const.value.kind {
            // NOTE: An anon const path expr is often lowered directly into a
            //       an HIR const arg with `ConstArgKind::Path`, meaning that
            //       there is no corresponding HIR id for the anon const's expr.
            //       Because of this, we skip checking the path expr's id, and
            //       only check the path itself.
            ast::ExprKind::Path(qself_ast, path_ast) => {
                self.visit_qself(qself_ast);
                self.visit_path(path_ast);
            }
            _ => ast::visit::walk_anon_const(self, anon_const),
        }
    }

    fn visit_block(&mut self, block: &'ast ast::Block) {
        self.check_node_id("block", block.id, block.span);

        let mut i = 0;
        let last_index = block.stmts.len() - 1;
        for stmt in &block.stmts {
            // NOTE: A trailing expression is represented as an expression statement in the AST,
            //       however the HIR has a dedicated block expression node with no statement node,
            //       meaning that no mapping can be created for the AST statement node directly.
            if i == last_index {
                if let ast::StmtKind::Expr(expr) = &stmt.kind {
                    self.visit_expr(expr);
                    continue;
                }
            }
            i += 1;

            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast ast::Stmt) {
        match &stmt.kind {
            | ast::StmtKind::Item(_)
            | ast::StmtKind::Empty
            | ast::StmtKind::MacCall(_) => {}
            _ => self.check_node_id("statement", stmt.id, stmt.span),
        }

        ast::visit::walk_stmt(self, stmt)
    }

    fn visit_local(&mut self, local: &'ast ast::Local) {
        self.check_node_id("local", local.id, local.span);
        ast::visit::walk_local(self, local)
    }

    fn visit_expr(&mut self, expr: &'ast ast::Expr) {
        match &expr.kind {
            | ast::ExprKind::Paren(_)
            | ast::ExprKind::Underscore
            | ast::ExprKind::MacCall(_)
            | ast::ExprKind::Dummy
            | ast::ExprKind::Err(_) => {}
            _ => self.check_node_id("expression", expr.id, expr.span),
        }

        match &expr.kind {
            ast::ExprKind::MethodCall(method_call) => {
                self.check_node_id("method call", method_call.seg.id, method_call.span);
            }
            _ => {}
        }

        match &expr.kind {
            ast::ExprKind::Assign(lhs, rhs, _) => {
                let mut assigned_subexprs = vec![];
                visit::assigned_subexprs_in_expr_assign_lhs(self.def_res, lhs, &mut assigned_subexprs);
                for assigned_subexpr in assigned_subexprs {
                    self.visit_expr(assigned_subexpr);
                }

                self.visit_expr(rhs);
            }

            _ => ast::visit::walk_expr(self, expr),
        }
    }

    fn visit_expr_field(&mut self, expr_field: &'ast ast::ExprField)  {
        self.check_node_id("field expression", expr_field.id, expr_field.span);
        ast::visit::walk_expr_field(self, expr_field)
    }

    fn visit_inline_asm_sym(&mut self, inline_asm_sym: &'ast ast::InlineAsmSym) {
        self.check_node_id("inline assembly symbol", inline_asm_sym.id, inline_asm_sym.path.span);
        ast::visit::walk_inline_asm_sym(self, inline_asm_sym)
    }

    fn visit_arm(&mut self, arm: &'ast ast::Arm) {
        self.check_node_id("arm", arm.id, arm.span);
        ast::visit::walk_arm(self, arm)
    }

    fn visit_pat(&mut self, pat: &'ast ast::Pat) {
        match &pat.kind {
            | ast::PatKind::Paren(_)
            | ast::PatKind::Rest
            | ast::PatKind::MacCall(..)
            | ast::PatKind::Err(_) => {}
            _ => self.check_node_id("pattern", pat.id, pat.span),
        }

        ast::visit::walk_pat(self, pat)
    }

    fn visit_pat_field(&mut self, pat_field: &'ast ast::PatField) {
        self.check_node_id("field pattern", pat_field.id, pat_field.span);
        ast::visit::walk_pat_field(self, pat_field)
    }

    fn visit_ty(&mut self, ty: &'ast ast::Ty) {
        match &ty.kind {
            | ast::TyKind::Paren(_)
            | ast::TyKind::MacCall(..)
            | ast::TyKind::Dummy
            | ast::TyKind::Err(_) => {}
            _ => self.check_node_id("type", ty.id, ty.span),
        }

        match &ty.kind {
            ast::TyKind::ImplTrait(id, _) => {
                self.check_node_id("impl trait", *id, ty.span);
            }
            _ => {}
        }

        match &ty.kind {
            ast::TyKind::BareFn(bare_fn_ty_ast) => {
                for generic_param in &bare_fn_ty_ast.generic_params {
                    self.visit_generic_param(generic_param);
                }
                for param in &bare_fn_ty_ast.decl.inputs {
                    self.visit_ty(&param.ty);
                }
                self.visit_fn_ret_ty(&bare_fn_ty_ast.decl.output);
            }
            _ => ast::visit::walk_ty(self, ty),
        }
    }

    fn visit_path_segment(&mut self, path_segment: &'ast ast::PathSegment) {
        self.check_node_id("path segment", path_segment.id, path_segment.ident.span);
        ast::visit::walk_path_segment(self, path_segment)
    }

    fn visit_assoc_item_constraint(&mut self, assoc_item_constraint: &'ast ast::AssocItemConstraint) {
        self.check_node_id("associated item constraint", assoc_item_constraint.id, assoc_item_constraint.span);
        ast::visit::walk_assoc_item_constraint(self, assoc_item_constraint)
    }
}

pub fn validate_body_resolutions<'tcx>(body_res: &BodyResolutions<'tcx>, def_res: &DefResolutions, krate_ast: &ast::Crate) {
    let mut validator = BodyResValidator { def_res, body_res };
    ast::visit::Visitor::visit_crate(&mut validator, krate_ast);
}

struct AstBodyChildItemCollector<'ast> {
    result: Vec<ast::DefItem<'ast>>,
}

impl<'ast> ast::visit::Visitor<'ast> for AstBodyChildItemCollector<'ast> {
    fn visit_item(&mut self, item: &'ast ast::Item) {
        self.result.push(ast::DefItem::Item(item));
    }

    fn visit_foreign_item(&mut self, item: &'ast ast::ForeignItem) {
        self.result.push(ast::DefItem::ForeignItem(item));
    }

    fn visit_assoc_item(&mut self, item: &'ast ast::AssocItem, ctx: rustc_ast::visit::AssocCtxt) {
        self.result.push(ast::DefItem::AssocItem(item, ctx));
    }
}

fn ast_body_child_items<'ast>(block: &'ast ast::Block) -> Vec<ast::DefItem<'ast>> {
    use ast::visit::Visitor;

    let mut collector = AstBodyChildItemCollector { result: vec![] };
    collector.visit_block(block);
    collector.result
}

struct HirBodyChildItemCollector<'tcx> {
    tcx: TyCtxt<'tcx>,
    result: Vec<hir::DefItem<'tcx>>,
}

impl<'hir> hir::intravisit::Visitor<'hir> for HirBodyChildItemCollector<'hir> {
    type NestedFilter = rustc_middle::hir::nested_filter::All;

    fn maybe_tcx(&mut self) -> Self::MaybeTyCtxt {
        self.tcx
    }

    fn visit_item(&mut self, item: &'hir hir::Item<'hir>) {
        self.result.push(hir::DefItem::Item(item));
    }

    fn visit_foreign_item(&mut self, item: &'hir hir::ForeignItem<'hir>) {
        self.result.push(hir::DefItem::ForeignItem(item));
    }

    fn visit_trait_item(&mut self, item: &'hir hir::TraitItem<'hir>) {
        self.result.push(hir::DefItem::TraitItem(item));
    }

    fn visit_impl_item(&mut self, item: &'hir hir::ImplItem<'hir>) {
        self.result.push(hir::DefItem::ImplItem(item));
    }
}

fn hir_body_child_items<'tcx>(tcx: TyCtxt<'tcx>, body: &'tcx hir::Body<'tcx>) -> Vec<hir::DefItem<'tcx>> {
    use hir::intravisit::Visitor;

    let mut collector = HirBodyChildItemCollector { tcx, result: vec![] };
    collector.visit_body(body);
    collector.result
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct DefDisambiguator {
    pub kind: hir::DefPathData,
    pub disambiguator: usize,
}

fn disambiguate_hir_def_item_node_path_components<'tcx>(tcx: TyCtxt<'tcx>, path: &[hir::Node]) -> Vec<Option<DefDisambiguator>> {
    fn disambiguated_kind<'hir>(def_item: hir::DefItem<'hir>) -> Option<hir::DefPathData> {
        match def_item {
            hir::DefItem::Item(item) => match item.kind {
                hir::ItemKind::ExternCrate(_, ident) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::Use(_, _) => None,
                hir::ItemKind::Static(_, ident, _, _) => Some(hir::DefPathData::ValueNs(ident.name)),
                hir::ItemKind::Const(ident, _, _, _) => Some(hir::DefPathData::ValueNs(ident.name)),
                hir::ItemKind::Fn { ident, .. } => Some(hir::DefPathData::ValueNs(ident.name)),
                hir::ItemKind::Macro(ident, _, _) => Some(hir::DefPathData::MacroNs(ident.name)),
                hir::ItemKind::Mod(ident, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::ForeignMod { .. } => Some(hir::DefPathData::ForeignMod),
                hir::ItemKind::GlobalAsm { .. } => Some(hir::DefPathData::GlobalAsm),
                hir::ItemKind::TyAlias(ident, _, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::Enum(ident, _, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::Struct(ident, _, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::Union(ident, _, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::Trait(_, _, ident, _, _, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::TraitAlias(ident, _, _) => Some(hir::DefPathData::TypeNs(ident.name)),
                hir::ItemKind::Impl(_) => Some(hir::DefPathData::Impl),
            }
            hir::DefItem::ForeignItem(item) => match item.kind {
                hir::ForeignItemKind::Fn(_, _, _) => Some(hir::DefPathData::ValueNs(item.ident.name)),
                hir::ForeignItemKind::Static(_, _, _) => Some(hir::DefPathData::ValueNs(item.ident.name)),
                hir::ForeignItemKind::Type => Some(hir::DefPathData::TypeNs(item.ident.name)),
            }
            hir::DefItem::TraitItem(item) => match item.kind {
                hir::TraitItemKind::Const(_, _) => Some(hir::DefPathData::ValueNs(item.ident.name)),
                hir::TraitItemKind::Fn(_, _) => Some(hir::DefPathData::ValueNs(item.ident.name)),
                hir::TraitItemKind::Type(_, _) => Some(hir::DefPathData::TypeNs(item.ident.name)),
            }
            hir::DefItem::ImplItem(item) => match item.kind {
                hir::ImplItemKind::Const(_, _) => Some(hir::DefPathData::ValueNs(item.ident.name)),
                hir::ImplItemKind::Fn(_, _) => Some(hir::DefPathData::ValueNs(item.ident.name)),
                hir::ImplItemKind::Type(_) => Some(hir::DefPathData::TypeNs(item.ident.name)),
            }
        }
    }

    fn disambiguate_child_item<'hir, I>(def_item: hir::DefItem<'hir>, items: I) -> Option<DefDisambiguator>
    where
        I: IntoIterator<Item = hir::DefItem<'hir>>,
    {
        let kind = disambiguated_kind(def_item);

        let disambiguator = items.into_iter()
            .filter(|&item| disambiguated_kind(item) == kind)
            .position(|item| item.def_id() == def_item.def_id());

        match (kind, disambiguator) {
            (Some(kind), Some(disambiguator)) => Some(DefDisambiguator { kind, disambiguator }),
            _ => None,
        }
    }

    path.iter()
        .tuple_windows::<(_, _)>()
        .map(|(parent_node, current_node)| {
            let Some(current_def_item) = hir::DefItem::from_node(current_node) else { return None; };

            match parent_node {
                hir::Node::Crate(parent_mod) => {
                    let items = parent_mod.item_ids.iter().map(|item_id| tcx.hir_item(*item_id)).map(hir::DefItem::Item);
                    disambiguate_child_item(current_def_item, items)
                }
                hir::Node::Item(parent_item) => match &parent_item.kind {
                    hir::ItemKind::Mod(_, parent_mod) => {
                        let items = parent_mod.item_ids.iter().map(|item_id| tcx.hir_item(*item_id)).map(hir::DefItem::Item);
                        disambiguate_child_item(current_def_item, items)
                    }
                    hir::ItemKind::ForeignMod { items: parent_items, .. } => {
                        let items = parent_items.iter().map(|item_ref| tcx.hir_foreign_item(item_ref.id)).map(hir::DefItem::ForeignItem);
                        disambiguate_child_item(current_def_item, items)
                    }
                    hir::ItemKind::Trait(_, _, _, _, _, parent_items) => {
                        let items = parent_items.iter().map(|item_ref| tcx.hir_trait_item(item_ref.id)).map(hir::DefItem::TraitItem);
                        disambiguate_child_item(current_def_item, items)
                    }
                    hir::ItemKind::Impl(parent_impl) => {
                        let items = parent_impl.items.iter().map(|item_ref| tcx.hir_impl_item(item_ref.id)).map(hir::DefItem::ImplItem);
                        disambiguate_child_item(current_def_item, items)
                    }
                    | hir::ItemKind::Static(_, _, _, parent_body)
                    | hir::ItemKind::Const(_, _, _, parent_body)
                    | hir::ItemKind::Fn { body: parent_body, .. } => {
                        disambiguate_child_item(current_def_item, hir_body_child_items(tcx, tcx.hir_body(*parent_body)))
                    }
                    _ => None,
                }
                hir::Node::TraitItem(parent_item) => match &parent_item.kind {
                    | hir::TraitItemKind::Const(_, Some(parent_body))
                    | hir::TraitItemKind::Fn(_, hir::TraitFn::Provided(parent_body)) => {
                        disambiguate_child_item(current_def_item, hir_body_child_items(tcx, tcx.hir_body(*parent_body)))
                    }
                    _ => None,
                }
                hir::Node::ImplItem(parent_item) => match &parent_item.kind {
                    | hir::ImplItemKind::Const(_, parent_body)
                    | hir::ImplItemKind::Fn(_, parent_body) => {
                        disambiguate_child_item(current_def_item, hir_body_child_items(tcx, tcx.hir_body(*parent_body)))
                    }
                    _ => None,
                }
                _ => None,
            }
        })
        .collect()
}

struct AstDefFinder<'ast, 'hir> {
    path_stack: VecDeque<(hir::Node<'hir>, Option<DefDisambiguator>)>,
    result: Option<ast::DefItem<'ast>>,
}

fn find_hir_def_item_in_ast<'ast, 'hir, I>(item_hir: hir::DefItem<'hir>, disambiguator: Option<DefDisambiguator>, items_ast: I) -> Option<ast::DefItem<'ast>>
where
    I: IntoIterator<Item = ast::DefItem<'ast>>,
{
    macro matching_item {
        ($(|)? $($pat:pat_param)|+ $(if $guard:expr)? $(,)?) => {
            items_ast.into_iter().find(|&item_ast| matches!(&item_ast.kind(), $($pat)|+ $(if $guard)?))
        },
        ($(|)? $($pat:pat_param)|+ $(if $guard:expr)? => |$item_ast:ident| $filter:expr $(,)?) => {
            items_ast.into_iter().find(|&$item_ast| matches!(&$item_ast.kind(), $($pat)|+ $(if $guard)?) && $filter)
        },
    }

    match item_hir {
        hir::DefItem::Item(item_hir) => match &item_hir.kind {
            hir::ItemKind::ExternCrate(symbol_hir, _) => {
                matching_item!(ast::DefItemKind::ExternCrate(symbol_ast, _) if symbol_ast == symbol_hir)
            }
            hir::ItemKind::Use(_, _) => None,
            hir::ItemKind::Static(_, _, _, _) => {
                matching_item!(ast::DefItemKind::Static(_) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Const(_, _, _, _) => {
                matching_item!(ast::DefItemKind::Const(_) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Fn { .. } => {
                matching_item!(ast::DefItemKind::Fn(_) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Macro(_, def_hir, _) => {
                matching_item!(ast::DefItemKind::MacroDef(_, def_ast) if std::ptr::eq(*def_hir, *def_ast))
            }
            hir::ItemKind::Mod(_, _) => {
                matching_item!(ast::DefItemKind::Mod(_, _, _) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::ForeignMod { .. } => {
                let Some(disambiguator) = disambiguator else { return None; };

                let hir::DefPathData::ForeignMod = disambiguator.kind else { return None; };
                let index = disambiguator.disambiguator;

                items_ast.into_iter().filter(|&item_ast| matches!(&item_ast.kind(), ast::DefItemKind::ForeignMod(_))).nth(index)
            }
            hir::ItemKind::GlobalAsm { .. } => {
                let Some(disambiguator) = disambiguator else { return None; };

                let hir::DefPathData::GlobalAsm = disambiguator.kind else { return None; };
                let index = disambiguator.disambiguator;

                items_ast.into_iter().filter(|&item_ast| matches!(&item_ast.kind(), ast::DefItemKind::GlobalAsm(_))).nth(index)
            }
            hir::ItemKind::TyAlias(_, _, _) => {
                matching_item!(ast::DefItemKind::TyAlias(_) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Enum(_, _, _) => {
                matching_item!(ast::DefItemKind::Enum(_, _, _) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Struct(_, _, _) => {
                matching_item!(ast::DefItemKind::Struct(_, _, _) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Union(_ ,_, _) => {
                matching_item!(ast::DefItemKind::Union(_, _, _) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Trait(_, _, _, _, _, _) => {
                matching_item!(ast::DefItemKind::Trait(_) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::TraitAlias(_, _, _) => {
                matching_item!(ast::DefItemKind::TraitAlias(_, _, _) => |item_ast| Some(item_ast.ident()) == item_hir.kind.ident())
            }
            hir::ItemKind::Impl(_) => {
                let Some(disambiguator) = disambiguator else { return None; };

                let hir::DefPathData::Impl = disambiguator.kind else { return None; };
                let index = disambiguator.disambiguator;

                items_ast.into_iter().filter(|&item_ast| matches!(&item_ast.kind(), ast::DefItemKind::Impl(_))).nth(index)
            }
        }
        hir::DefItem::ForeignItem(item_hir) => match &item_hir.kind {
            hir::ForeignItemKind::Fn(_, _, _) => {
                matching_item!(ast::DefItemKind::Fn(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
            hir::ForeignItemKind::Static(_, _, _) => {
                matching_item!(ast::DefItemKind::Static(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
            hir::ForeignItemKind::Type => {
                matching_item!(ast::DefItemKind::TyAlias(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
        }
        hir::DefItem::TraitItem(item_hir) => match &item_hir.kind {
            hir::TraitItemKind::Const(_, _) => {
                matching_item!(ast::DefItemKind::Const(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
            hir::TraitItemKind::Fn(_, _) => {
                matching_item!(ast::DefItemKind::Fn(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
            hir::TraitItemKind::Type(_, _) => {
                matching_item!(ast::DefItemKind::TyAlias(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
        }
        hir::DefItem::ImplItem(item_hir) => match &item_hir.kind {
            hir::ImplItemKind::Const(_, _) => {
                matching_item!(ast::DefItemKind::Const(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
            hir::ImplItemKind::Fn(_, _) => {
                matching_item!(ast::DefItemKind::Fn(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
            hir::ImplItemKind::Type(_) => {
                matching_item!(ast::DefItemKind::TyAlias(_) => |item_ast| item_ast.ident() == item_hir.ident)
            }
        }
    }
}

impl<'ast, 'hir> ast::visit::Visitor<'ast> for AstDefFinder<'ast, 'hir> {
    fn visit_crate(&mut self, krate: &'ast ast::Crate) {
        let Some(&(node_hir, None)) = self.path_stack.front() else { return; };

        let hir::Node::Crate(_) = node_hir else { return; };
        self.path_stack.pop_front();

        let Some(&(node_hir, disambiguator)) = self.path_stack.front() else { return; };
        let Some(def_item_hir) = hir::DefItem::from_node(&node_hir) else { return; };

        let def_item_ast = match def_item_hir {
            hir::DefItem::Item(_) => {
                let def_items_ast = krate.items.iter().map(Deref::deref).map(ast::DefItem::Item);
                find_hir_def_item_in_ast(def_item_hir, disambiguator, def_items_ast)
            }
            _ => { return; }
        };

        let Some(def_item_ast) = def_item_ast else { return; };
        self.path_stack.pop_front();

        if self.path_stack.is_empty() {
            self.result = Some(def_item_ast);
            return;
        }

        match def_item_ast {
            ast::DefItem::Item(item_ast) => self.visit_item(item_ast),
            ast::DefItem::ForeignItem(item_ast) => self.visit_foreign_item(item_ast),
            ast::DefItem::AssocItem(item_ast, assoc_ctx_ast) => self.visit_assoc_item(item_ast, assoc_ctx_ast),
        }
    }

    fn visit_item(&mut self, item: &'ast ast::Item) {
        let Some(&(node_hir, disambiguator)) = self.path_stack.front() else { return; };
        let Some(def_item_hir) = hir::DefItem::from_node(&node_hir) else { return; };

        let def_item_ast = match (def_item_hir, &item.kind) {
            (hir::DefItem::Item(_), ast::ItemKind::Mod(_, _, ast::ModKind::Loaded(items_ast, _, _, _))) => {
                let def_items_ast = items_ast.iter().map(Deref::deref).map(ast::DefItem::Item);
                find_hir_def_item_in_ast(def_item_hir, disambiguator, def_items_ast)
            }
            (hir::DefItem::ForeignItem(_), ast::ItemKind::ForeignMod(foreign_mod_ast)) => {
                let def_items_ast = foreign_mod_ast.items.iter().map(Deref::deref).map(ast::DefItem::ForeignItem);
                find_hir_def_item_in_ast(def_item_hir, disambiguator, def_items_ast)
            }
            (hir::DefItem::TraitItem(_), ast::ItemKind::Trait(trait_ast)) => {
                let def_items_ast = trait_ast.items.iter().map(Deref::deref)
                    .map(|item_ast| ast::DefItem::AssocItem(item_ast, ast::visit::AssocCtxt::Trait));
                find_hir_def_item_in_ast(def_item_hir, disambiguator, def_items_ast)
            }
            (hir::DefItem::ImplItem(_), ast::ItemKind::Impl(impl_ast)) => {
                let def_items_ast = impl_ast.items.iter().map(Deref::deref)
                    .map(|item_ast| ast::DefItem::AssocItem(item_ast, ast::visit::AssocCtxt::Impl { of_trait: impl_ast.of_trait.is_some() }));
                find_hir_def_item_in_ast(def_item_hir, disambiguator, def_items_ast)
            }
            (hir::DefItem::Item(_), ast::ItemKind::Fn(fn_ast)) => {
                let Some(body_ast) = &fn_ast.body else { return; };
                find_hir_def_item_in_ast(def_item_hir, disambiguator, ast_body_child_items(body_ast))
            }
            _ => { return; }
        };

        let Some(def_item_ast) = def_item_ast else { return; };
        self.path_stack.pop_front();

        if self.path_stack.is_empty() {
            self.result = Some(def_item_ast);
            return;
        }

        match def_item_ast {
            ast::DefItem::Item(item_ast) => self.visit_item(item_ast),
            ast::DefItem::ForeignItem(item_ast) => self.visit_foreign_item(item_ast),
            ast::DefItem::AssocItem(item_ast, assoc_ctx_ast) => self.visit_assoc_item(item_ast, assoc_ctx_ast),
        }
    }

    fn visit_foreign_item(&mut self, item: &'ast ast::ForeignItem) {
        let Some(&(node_hir, disambiguator)) = self.path_stack.front() else { return; };
        let Some(def_item_hir) = hir::DefItem::from_node(&node_hir) else { return; };

        let def_item_ast = match (def_item_hir, &item.kind) {
            (hir::DefItem::Item(_), ast::ForeignItemKind::Fn(fn_ast)) => {
                let Some(body_ast) = &fn_ast.body else { return; };
                find_hir_def_item_in_ast(def_item_hir, disambiguator, ast_body_child_items(body_ast))
            }
            _ => { return; }
        };

        let Some(def_item_ast) = def_item_ast else { return; };
        self.path_stack.pop_front();

        if self.path_stack.is_empty() {
            self.result = Some(def_item_ast);
            return;
        }

        match def_item_ast {
            ast::DefItem::Item(item_ast) => self.visit_item(item_ast),
            ast::DefItem::ForeignItem(item_ast) => self.visit_foreign_item(item_ast),
            ast::DefItem::AssocItem(item_ast, assoc_ctx_ast) => self.visit_assoc_item(item_ast, assoc_ctx_ast),
        }
    }

    fn visit_assoc_item(&mut self, item: &'ast ast::AssocItem, _ctx: ast::visit::AssocCtxt) {
        let Some(&(node_hir, disambiguator)) = self.path_stack.front() else { return; };
        let Some(def_item_hir) = hir::DefItem::from_node(&node_hir) else { return; };

        let def_item_ast = match (def_item_hir, &item.kind) {
            (hir::DefItem::Item(_), ast::AssocItemKind::Fn(fn_ast)) => {
                let Some(body_ast) = &fn_ast.body else { return; };
                find_hir_def_item_in_ast(def_item_hir, disambiguator, ast_body_child_items(body_ast))
            }
            _ => { return; }
        };

        let Some(def_item_ast) = def_item_ast else { return; };
        self.path_stack.pop_front();

        if self.path_stack.is_empty() {
            self.result = Some(def_item_ast);
            return;
        }

        match def_item_ast {
            ast::DefItem::Item(item_ast) => self.visit_item(item_ast),
            ast::DefItem::ForeignItem(item_ast) => self.visit_foreign_item(item_ast),
            ast::DefItem::AssocItem(item_ast, assoc_ctx_ast) => self.visit_assoc_item(item_ast, assoc_ctx_ast),
        }
    }
}

pub fn find_def_in_ast<'ast, 'tcx>(tcx: TyCtxt<'tcx>, def_res: &DefResolutions, def_id: hir::LocalDefId, krate: &'ast ast::Crate) -> Option<ast::DefItem<'ast>> {
    use ast::visit::Visitor;

    // The definition HIR path will contain the full HIR tree walk, including blocks, statements, expressions, etc.
    // (e.g. for items nested inside function bodies). However, it is sufficient to only look at the item-like path
    // components to resolve definitions. This is also how the compiler's own `DefPath`s are defined.
    let def_hir_item_node_path = res::def_hir_path(tcx, def_id).into_iter()
        .map(|(_, node)| node)
        .filter(|node| matches!(node, hir::Node::Crate(_)) || hir::DefItem::from_node(node).is_some())
        .collect::<Vec<_>>();

    let disambiguators = iter::once(None).chain(disambiguate_hir_def_item_node_path_components(tcx, &def_hir_item_node_path));
    let path_stack = iter::zip(def_hir_item_node_path, disambiguators).collect::<VecDeque<_>>();

    let mut finder = AstDefFinder {
        path_stack,
        result: None,
    };
    finder.visit_crate(krate);

    if let Some(def_item) = finder.result {
        let found_def_id = def_res.node_id_to_def_id.get(&def_item.node_id()).copied();
        if found_def_id != Some(def_id) {
            span_bug!(def_item.span(), "found mismatched syntax item for {}", tcx.def_path_str(def_id));
        }
    }

    finder.result
}
