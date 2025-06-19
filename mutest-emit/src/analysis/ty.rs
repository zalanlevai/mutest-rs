pub use rustc_middle::ty::*;

use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::ty;
use rustc_trait_selection::infer::InferCtxtExt;

use crate::analysis::hir;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol};

pub fn impls_trait<'tcx>(tcx: TyCtxt<'tcx>, body_def_id: hir::LocalDefId, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: Vec<ty::GenericArg<'tcx>>) -> bool {
    let ty = tcx.erase_regions(ty);
    if ty.has_escaping_bound_vars() { return false; }

    let infcx = tcx.infer_ctxt().build(ty::TypingMode::analysis_in_body(tcx, body_def_id));
    let param_env = tcx.param_env(body_def_id);
    infcx.type_implements_trait(trait_def_id, tcx.mk_args_trait(ty, args), param_env).must_apply_modulo_regions()
}

pub fn impl_assoc_ty<'tcx>(tcx: TyCtxt<'tcx>, caller_def_id: hir::LocalDefId, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: Vec<ty::GenericArg<'tcx>>, assoc_ty: Symbol) -> Option<Ty<'tcx>> {
    let typing_env = ty::TypingEnv::post_analysis(tcx, caller_def_id);

    tcx.associated_items(trait_def_id)
        .find_by_ident_and_kind(tcx, Ident::new(assoc_ty, DUMMY_SP), ty::AssocTag::Type, trait_def_id)
        .and_then(|assoc_item| {
            let args = tcx.mk_args_trait(ty, args);
            let proj = Ty::new_projection(tcx, assoc_item.def_id, args);
            tcx.try_normalize_erasing_regions(typing_env, proj).ok()
        })
}

trait SpanFromGenericsExt {
    fn span_from_generics<'tcx>(self, tcx: TyCtxt<'tcx>, item_with_generics: hir::DefId) -> Span;
}

impl SpanFromGenericsExt for ty::ParamConst {
    fn span_from_generics<'tcx>(self, tcx: TyCtxt<'tcx>, item_with_generics: hir::DefId) -> Span {
        let generics = tcx.generics_of(item_with_generics);
        let const_param = generics.const_param(self, tcx);
        tcx.def_span(const_param.def_id)
    }
}

pub use print::ast_repr;

pub mod print {
    use std::iter;

    use rustc_infer::infer::TyCtxtInferExt;
    use rustc_middle::mir;
    use rustc_middle::ty::{self, Ty, TyCtxt};
    use rustc_session::cstore::{ExternCrate, ExternCrateSource};
    use thin_vec::ThinVec;

    use crate::analysis::ast_lowering;
    use crate::analysis::hir::{self, LOCAL_CRATE};
    use crate::analysis::res;
    use crate::codegen::ast::{self, P};
    use crate::codegen::hygiene;
    use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym, kw};

    use super::SpanFromGenericsExt;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum ScopedItemPaths {
        FullyQualified,
        Trimmed,
    }

    pub trait Printer<'tcx> {
        type Error;

        type Type;
        type DynExistential;
        type Const;
        type Region;
        type Path;

        fn tcx(&self) -> TyCtxt<'tcx>;
        fn crate_res(&self) -> &res::CrateResolutions<'tcx>;

        fn path_crate(&mut self, cnum: hir::CrateNum) -> Result<Self::Path, Self::Error>;
        fn path_append(&mut self, path: Self::Path, disambiguated_data: &hir::DisambiguatedDefPathData) -> Result<Self::Path, Self::Error>;
        fn path_generic_args(
            &mut self,
            path: Self::Path,
            args: &[ty::GenericArg<'tcx>],
            assoc_constraints: impl Iterator<Item = ty::ExistentialProjection<'tcx>>,
        ) -> Result<Self::Path, Self::Error>;

        fn print_res_def_path(&mut self, def_path: res::DefPath<'tcx>) -> Result<Self::Path, Self::Error>;

        fn try_print_visible_def_path(&mut self, def_id: hir::DefId, scope: Option<hir::DefId>, scoped_item_paths: ScopedItemPaths) -> Result<Option<Self::Path>, Self::Error> {
            fn try_print_visible_def_path_impl<'tcx, T: Printer<'tcx> + ?Sized>(
                printer: &mut T,
                def_id: hir::DefId,
                scope: Option<hir::DefId>,
                scoped_item_paths: ScopedItemPaths,
                callers: &mut Vec<hir::DefId>,
            ) -> Result<Option<T::Path>, T::Error> {
                if let Some(cnum) = def_id.as_crate_root() {
                    if cnum == LOCAL_CRATE {
                        return Ok(Some(printer.path_crate(cnum)?));
                    }

                    match printer.tcx().extern_crate(cnum) {
                        Some(&ExternCrate { src, dependency_of, span, .. }) => match (src, dependency_of) {
                            // Crate loaded by implicitly injected `extern crate core` / `extern crate std`.
                            (ExternCrateSource::Extern(_), LOCAL_CRATE) if span.is_dummy() => {
                                return Ok(Some(printer.path_crate(cnum)?));
                            }
                            // Crate loaded by `extern crate` declaration.
                            (ExternCrateSource::Extern(def_id), LOCAL_CRATE) => {
                                return Ok(Some(printer.print_def_path(def_id, &[])?));
                            }
                            // Crate implicitly loaded by a path resolving through extern prelude.
                            (ExternCrateSource::Path, LOCAL_CRATE) => {
                                return Ok(Some(printer.path_crate(cnum)?));
                            }
                            _ => {}
                        }
                        None => {
                            return Ok(Some(printer.path_crate(cnum)?));
                        }
                    }
                }

                if let Some(visible_def_path) = res::visible_def_path(printer.tcx(), printer.crate_res(), def_id, scope, None, DUMMY_SP) {
                    return Some(printer.print_res_def_path(visible_def_path)).transpose();
                }
                if let Some(scope) = scope {
                    let visible_def_path = res::locally_visible_def_path(printer.tcx(), def_id, scope);
                    if let Ok(visible_def_path) = visible_def_path {
                        return Some(printer.print_res_def_path(visible_def_path)).transpose();
                    }
                }

                let visible_parent_map = printer.tcx().visible_parent_map(());
                let mut cur_def_key = printer.tcx().def_key(def_id);

                // Constructors are unnamed by themselves. We must use the name of their parent instead.
                if let hir::DefPathData::Ctor = cur_def_key.disambiguated_data.data {
                    let parent = hir::DefId {
                        krate: def_id.krate,
                        index: cur_def_key.parent.expect("constructor without a parent"),
                    };

                    cur_def_key = printer.tcx().def_key(parent);
                }

                let Some(visible_parent) = visible_parent_map.get(&def_id).cloned() else { return Ok(None); };
                let actual_parent = printer.tcx().opt_parent(def_id);

                let mut data = cur_def_key.disambiguated_data.data;

                match data {
                    hir::DefPathData::TypeNs(ref mut name) if Some(visible_parent) != actual_parent => {
                        // Item might be re-exported several times, but filter for the ones that are public and whose
                        // identifier is not `_`.
                        let reexport = printer.tcx().module_children(visible_parent).iter()
                            .filter(|child| child.res.opt_def_id() == Some(def_id))
                            .find(|child| child.vis.is_public() && child.ident.name != kw::Underscore)
                            .map(|child| child.ident.name);

                        let Some(new_name) = reexport else { return Ok(None); };
                        *name = new_name;
                    }
                    // Re-exported `extern crate`.
                    hir::DefPathData::CrateRoot => {
                        data = hir::DefPathData::TypeNs(printer.tcx().crate_name(def_id.krate));
                    }
                    _ => {}
                }

                if callers.contains(&visible_parent) { return Ok(None); }
                callers.push(visible_parent);
                let Some(path) = try_print_visible_def_path_impl(printer, visible_parent, scope, scoped_item_paths, callers)? else { return Ok(None); };
                callers.pop();

                let disambiguated_data = hir::DisambiguatedDefPathData { data, disambiguator: 0 };
                let path = printer.path_append(path, &disambiguated_data)?;
                Ok(Some(path))
            }

            let mut callers = vec![];
            try_print_visible_def_path_impl(self, def_id, scope, scoped_item_paths, &mut callers)
        }

        fn print_def_path(&mut self, def_id: hir::DefId, args: &'tcx [ty::GenericArg<'tcx>]) -> Result<Self::Path, Self::Error>;

        fn print_region(&mut self, region: ty::Region<'tcx>) -> Result<Self::Region, Self::Error>;
        fn print_const(&mut self, ct: ty::Const<'tcx>) -> Result<Self::Const, Self::Error>;
        fn print_dyn_existential(&mut self, predicates: &'tcx ty::List<ty::PolyExistentialPredicate<'tcx>>) -> Result<Self::DynExistential, Self::Error>;
        fn print_ty(&mut self, ty: Ty<'tcx>) -> Result<Self::Type, Self::Error>;
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum DefPathHandling {
        PreferVisible(ScopedItemPaths),
        ForceVisible(ScopedItemPaths),
        FullyQualified,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum OpaqueTyHandling {
        Keep,
        Infer,
        Resolve,
    }

    #[derive(Clone, Copy)]
    pub struct AstTyPrinter<'tcx, 'op> {
        tcx: TyCtxt<'tcx>,
        crate_res: &'op res::CrateResolutions<'tcx>,
        def_res: &'op ast_lowering::DefResolutions,
        scope: Option<hir::DefId>,
        sp: Span,
        def_path_handling: DefPathHandling,
        opaque_ty_handling: OpaqueTyHandling,
        sanitize_macro_expns: bool,
        binding_item_def_id: hir::DefId,
    }

    impl<'tcx, 'op> Printer<'tcx> for AstTyPrinter<'tcx, 'op> {
        type Error = String;

        type Type = P<ast::Ty>;
        type DynExistential = P<ast::Ty>;
        type Const = ast::AnonConst;
        type Region = Option<ast::Lifetime>;
        type Path = ast::Path;

        fn tcx(&self) -> TyCtxt<'tcx> {
            self.tcx
        }

        fn crate_res(&self) -> &res::CrateResolutions<'tcx> {
            self.crate_res
        }

        fn path_crate(&mut self, cnum: hir::CrateNum) -> Result<Self::Path, Self::Error> {
            match cnum {
                LOCAL_CRATE => Ok(ast::mk::path_ident(self.sp, Ident::new(kw::Crate, self.sp))),
                _ => {
                    let crate_name = self.crate_res.visible_crate_name(cnum);
                    Ok(ast::mk::path_ident(self.sp, Ident::new(crate_name, self.sp)))
                }
            }
        }

        fn path_append(&mut self, mut path: Self::Path, disambiguated_data: &hir::DisambiguatedDefPathData) -> Result<Self::Path, Self::Error> {
            let hir::DefPathDataName::Named(name) = disambiguated_data.data.name() else {
                return Err("encountered anonymous, ambiguous path segment".to_owned());
            };

            path = ast::mk::pathx(self.sp, path, vec![Ident::new(name, self.sp)]);

            Ok(path)
        }

        fn path_generic_args(
            &mut self,
            mut path: Self::Path,
            args: &[ty::GenericArg<'tcx>],
            assoc_constraints: impl Iterator<Item = ty::ExistentialProjection<'tcx>>,
        ) -> Result<Self::Path, Self::Error> {
            let sp = self.sp;

            let mut errors = vec![];

            let mut args_ast = args.iter()
                .map(|arg: &ty::GenericArg<'tcx>| -> Result<Option<ast::AngleBracketedArg>, Self::Error> {
                    match arg.kind() {
                        ty::GenericArgKind::Type(ty) => {
                            let ty_ast = match self.print_ty(ty) {
                                Ok(ty_ast) => ty_ast,
                                Err(error) => {
                                    let mut diagnostic = self.tcx.dcx().struct_span_warn(sp, format!("replacing unrepresentable generic type argument `{ty}` with `_`"));
                                    diagnostic.span_label(sp, error);
                                    diagnostic.emit();

                                    ast::mk::ty(sp, ast::TyKind::Infer)
                                }
                            };
                            Ok(Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Type(ty_ast))))
                        }
                        ty::GenericArgKind::Const(ct) => {
                            let const_ast = self.print_const(ct)?;
                            Ok(Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Const(const_ast))))
                        }
                        ty::GenericArgKind::Lifetime(region) => {
                            let Some(lifetime) = self.print_region(region)? else { return Ok(None); };
                            Ok(Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Lifetime(lifetime))))
                        }
                    }
                })
                .filter_map(|res| {
                    if let Err(err) = res { errors.push(err); return None; }
                    res.ok().flatten()
                })
                .collect::<ThinVec<_>>();

            assoc_constraints
                .map(|assoc_constraint| -> Result<_, Self::Error> {
                    let name = self.tcx.associated_item(assoc_constraint.def_id).name();

                    let term_ast = match assoc_constraint.term.kind() {
                        ty::TermKind::Ty(ty) => ast::Term::Ty(self.print_ty(ty)?),
                        ty::TermKind::Const(ct) => ast::Term::Const(self.print_const(ct)?),
                    };

                    Ok(ast::AngleBracketedArg::Constraint(ast::AssocItemConstraint {
                        id: ast::DUMMY_NODE_ID,
                        ident: Ident::new(name, self.sp),
                        kind: ast::AssocItemConstraintKind::Equality { term: term_ast },
                        gen_args: None,
                        span: self.sp,
                    }))
                })
                .filter_map(|res| {
                    if let Err(err) = res { errors.push(err); return None; }
                    res.ok()
                })
                .collect_into(&mut args_ast);

            match errors.len() {
                0 => {}
                1 => return Err(errors.remove(0)),
                _ => return Err(format!("encountered {errors} errors: {errs}",
                    errors = errors.len(),
                    errs = errors.into_iter().intersperse("; ".to_owned()).collect::<String>(),
                )),
            }

            let Some(last_segment) = path.segments.last_mut() else { return Err("encountered empty path".to_owned()) };
            last_segment.args = Some(P(ast::GenericArgs::AngleBracketed(ast::AngleBracketedArgs { span: self.sp, args: args_ast })));

            Ok(path)
        }

        fn print_res_def_path(&mut self, def_path: res::DefPath<'tcx>) -> Result<Self::Path, Self::Error> {
            let (None, path) = def_path.ast_path(self.crate_res, self) else {
                return Err("encountered unresolved definition with qualified path in type position".to_owned());
            };

            Ok(path)
        }

        fn print_def_path(&mut self, def_id: hir::DefId, args: &'tcx [ty::GenericArg<'tcx>]) -> Result<Self::Path, Self::Error> {
            let mut path = 'path: {
                if let DefPathHandling::PreferVisible(scoped_item_paths) | DefPathHandling::ForceVisible(scoped_item_paths) = self.def_path_handling {
                    if let Some(path) = self.try_print_visible_def_path(def_id, self.scope, scoped_item_paths)? {
                        break 'path Ok(path);
                    }

                    if let DefPathHandling::ForceVisible(_) = self.def_path_handling {
                        break 'path Err("cannot find visible path for definition".to_owned());
                    }
                }

                // FIXME
                Err(format!("encountered definition `{}` with no visible path", self.tcx.def_path_str(def_id)))
            }?;

            if self.sanitize_macro_expns {
                // HACK: This is inefficient, as it resolves the path again, which already happens in `try_print_visible_def_path`.
                // TODO: Remove most code in `try_print_visible_def_path` and just use the logic in the `hygiene` module once sanitization becomes the default.
                hygiene::sanitize_path(self.tcx, self.crate_res, self.def_res, self.scope, &mut path, hir::Res::Def(self.tcx.def_kind(def_id), def_id), false);
            }

            if args.is_empty() { return Ok(path); }
            let item_args = self.tcx().generics_of(def_id).own_args(args);
            if !item_args.is_empty() {
                path = self.path_generic_args(path, item_args, iter::empty())?;
            }

            // NOTE: If the def is a trait assoc item, then we must also write out the trait generics on the trait path segment.
            if let Some(assoc_item) = self.tcx.opt_associated_item(def_id) && let Some(trait_def_id) = assoc_item.trait_container(self.tcx) {
                // HACK: Temporarily turn the path into a path to the containing trait itself to append generic args to it.
                let Some(assoc_item_path_segment) = path.segments.pop() else { unreachable!("empty path") };

                let trait_args = self.tcx().generics_of(trait_def_id).own_args(args);
                if !trait_args.is_empty() {
                    path = self.path_generic_args(path, trait_args, iter::empty())?;
                }

                path.segments.push(assoc_item_path_segment);
            }

            Ok(path)
        }

        fn print_region(&mut self, region: ty::Region<'tcx>) -> Result<Self::Region, Self::Error> {
            let sp = self.sp;

            match region.kind() {
                ty::RegionKind::ReStatic => Ok(Some(ast::mk::lifetime(sp, Ident::new(kw::StaticLifetime, sp)))),

                ty::RegionKind::ReEarlyParam(early_param_region) => {
                    if early_param_region.name == sym::empty { return Ok(None); }

                    let mut ident = Ident::new(early_param_region.name, sp);
                    if self.sanitize_macro_expns {
                        let def_ident_span = match region.opt_param_def_id(self.tcx, self.binding_item_def_id) {
                            Some(def_id) => self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP),
                            None => DUMMY_SP,
                        };
                        hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                    }
                    Ok(Some(ast::mk::lifetime(sp, ident)))
                }

                | ty::RegionKind::ReBound(_, ty::BoundRegion { kind: bound_region_kind, .. })
                | ty::RegionKind::RePlaceholder(ty::Placeholder { bound: ty::BoundRegion { kind: bound_region_kind, .. }, .. })
                => {
                    let ty::BoundRegionKind::Named(def_id, region_name) = bound_region_kind else { return Ok(None); };
                    if region_name == sym::empty || region_name == kw::UnderscoreLifetime { return Ok(None); }

                    let mut ident = Ident::new(region_name, sp);
                    if self.sanitize_macro_expns {
                        let def_ident_span = self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                        hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                    }
                    Ok(Some(ast::mk::lifetime(sp, ident)))
                }

                ty::RegionKind::ReLateParam(ty::LateParamRegion { kind: late_param_region_kind, .. }) => {
                    let ty::LateParamRegionKind::Named(def_id, region_name) = late_param_region_kind else { return Ok(None); };
                    if region_name == sym::empty || region_name == kw::UnderscoreLifetime { return Ok(None); }

                    let mut ident = Ident::new(region_name, sp);
                    if self.sanitize_macro_expns {
                        let def_ident_span = self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                        hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                    }
                    Ok(Some(ast::mk::lifetime(sp, ident)))
                }

                ty::RegionKind::ReVar(_) | ty::RegionKind::ReErased => Ok(None),

                ty::RegionKind::ReError(_) => Err("encountered region error".to_owned()),
            }
        }

        fn print_const(&mut self, ct: ty::Const<'tcx>) -> Result<Self::Const, Self::Error> {
            let sp = self.sp;

            match ct.kind() {
                ty::ConstKind::Param(param_const) => {
                    let mut ident = Ident::new(param_const.name, sp);
                    if self.sanitize_macro_expns {
                        'sanitize: {
                            let Some(scope) = self.scope else { break 'sanitize; };
                            let def_ident_span = param_const.span_from_generics(self.tcx, scope);
                            hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                        }
                    }

                    Ok(ast::mk::anon_const(sp, ast::mk::expr_ident(sp, ident).into_inner().kind))
                }

                | ty::ConstKind::Infer(_)
                | ty::ConstKind::Bound(_, _)
                | ty::ConstKind::Placeholder(_)
                | ty::ConstKind::Unevaluated(_)
                | ty::ConstKind::Value(_)
                | ty::ConstKind::Expr(_)
                => {
                    let infcx = self.tcx.infer_ctxt().build(ty::TypingMode::PostAnalysis);
                    let value = rustc_trait_selection::traits::evaluate_const(&infcx, ct, ty::ParamEnv::empty()).try_to_value().ok_or_else(|| "encountered invalid const".to_owned())?;
                    let val = self.tcx.valtree_to_const_val(value);

                    match val {
                        mir::ConstValue::Scalar(scalar) => {
                            let lit_expr = match value.ty.kind() {
                                ty::TyKind::Bool => {
                                    scalar.to_bool().map(|v| ast::mk::expr_bool(sp, v))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid bool const: {e:?}"))
                                }
                                ty::TyKind::Char => {
                                    scalar.to_char().map(|v| ast::mk::expr_lit(sp, ast::token::LitKind::Char, Symbol::intern(&v.to_string()), None))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid char const: {e:?}"))
                                }
                                ty::TyKind::Int(ty::IntTy::I8) => {
                                    scalar.to_i8().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i8))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid i8 const: {e:?}"))
                                }
                                ty::TyKind::Int(ty::IntTy::I16) => {
                                    scalar.to_i16().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i16))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid i16 const: {e:?}"))
                                }
                                ty::TyKind::Int(ty::IntTy::I32) => {
                                    scalar.to_i32().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i32))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid i32 const: {e:?}"))
                                }
                                ty::TyKind::Int(ty::IntTy::I64) => {
                                    scalar.to_i64().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i64))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid i64 const: {e:?}"))
                                }
                                ty::TyKind::Int(ty::IntTy::I128) => {
                                    scalar.to_i128().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i128))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid i128 const: {e:?}"))
                                }
                                ty::TyKind::Int(ty::IntTy::Isize) => {
                                    scalar.to_target_isize(&self.tcx).map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::isize))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid isize const: {e:?}"))
                                }
                                ty::TyKind::Uint(ty::UintTy::U8) => {
                                    scalar.to_u8().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u8))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid u8 const: {e:?}"))
                                }
                                ty::TyKind::Uint(ty::UintTy::U16) => {
                                    scalar.to_u16().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u16))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid u16 const: {e:?}"))
                                }
                                ty::TyKind::Uint(ty::UintTy::U32) => {
                                    scalar.to_u32().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u32))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid u32 const: {e:?}"))
                                }
                                ty::TyKind::Uint(ty::UintTy::U64) => {
                                    scalar.to_u64().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u64))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid u64 const: {e:?}"))
                                }
                                ty::TyKind::Uint(ty::UintTy::U128) => {
                                    scalar.to_u128().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u128))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid u128 const: {e:?}"))
                                }
                                ty::TyKind::Uint(ty::UintTy::Usize) => {
                                    scalar.to_target_usize(&self.tcx).map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::usize))
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid usize const: {e:?}"))
                                }
                                ty::TyKind::Float(ty::FloatTy::F16) => {
                                    scalar.to_f16()
                                        .map(|v| {
                                            let v = f16::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u16);
                                            ast::mk::expr_float_exact(sp, v as f64, sym::f16)
                                        })
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid f16 const: {e:?}"))
                                }
                                ty::TyKind::Float(ty::FloatTy::F32) => {
                                    scalar.to_f32()
                                        .map(|v| {
                                            let v = f32::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u32);
                                            ast::mk::expr_float_exact(sp, v as f64, sym::f32)
                                        })
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid f32 const: {e:?}"))
                                }
                                ty::TyKind::Float(ty::FloatTy::F64) => {
                                    scalar.to_f64()
                                        .map(|v| {
                                            let v = f64::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u64);
                                            ast::mk::expr_float_exact(sp, v, sym::f64)
                                        })
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid f64 const: {e:?}"))
                                }
                                ty::TyKind::Float(ty::FloatTy::F128) => {
                                    scalar.to_f128()
                                        .map(|v| {
                                            let rounded_v: rustc_apfloat::ieee::Double = rustc_apfloat::FloatConvert::convert(v, &mut false).value;
                                            let v = f64::from_bits(rustc_apfloat::ieee::Semantics::to_bits(rounded_v) as u64);
                                            ast::mk::expr_float_exact(sp, v, sym::f128)
                                        })
                                        .report_err()
                                        .map_err(|e| format!("encountered invalid f128 const: {e:?}"))
                                }
                                _ => Err("encountered unknown constant scalar value".to_owned())
                            }?;

                            Ok(ast::mk::anon_const(sp, lit_expr.into_inner().kind))
                        }

                        mir::ConstValue::ZeroSized => Err("encountered zero-sized const".to_owned()),
                        mir::ConstValue::Slice { .. } => Err("encountered slice const".to_owned()),
                        mir::ConstValue::Indirect { .. } => Err("encountered indirect const".to_owned()),
                    }
                }

                ty::ConstKind::Error(_) => Err("encountered const error".to_owned()),
            }
        }

        fn print_dyn_existential(&mut self, predicates: &'tcx ty::List<ty::PolyExistentialPredicate<'tcx>>) -> Result<Self::DynExistential, Self::Error> {
            let sp = self.sp;

            let principal = predicates.principal().map_or(Ok(None), |principal| -> Result<_, Self::Error> {
                let principal = principal.skip_binder();

                let def_path = self.print_def_path(principal.def_id, &[])?;

                // Fn(...) -> ...
                if let Some(_) = self.tcx.fn_trait_kind_from_def_id(principal.def_id)
                    && let ty::Tuple(input_tys) = principal.args.type_at(0).kind()
                    && let mut projections = predicates.projection_bounds()
                    && let (Some(projection), None) = (projections.next(), projections.next())
                {
                    let output_ty = projection.skip_binder().term.as_type();

                    let input_tys_ast = input_tys.iter().map(|ty| self.print_ty(ty)).try_collect()?;
                    let output_ty_ast = output_ty.map_or(Result::<_, Self::Error>::Ok(None), |ty| Ok(Some(self.print_ty(ty)?)))?;
                    let args = ast::mk::parenthesized_args(sp, input_tys_ast, output_ty_ast);
                    let path = ast::mk::pathx_raw(sp, def_path, vec![], Some(args));
                    return Ok(Some(ast::mk::trait_bound(ast::TraitBoundModifiers::NONE, path)));
                }

                let dummy_self_ty = Ty::new_fresh(self.tcx, 0);
                let principal = principal.with_self_ty(self.tcx, dummy_self_ty);

                let args = self.tcx.generics_of(principal.def_id).own_args_no_defaults(self.tcx, principal.args);
                let assoc_constraints = predicates.projection_bounds().map(|bounds| bounds.skip_binder());
                let path = self.path_generic_args(def_path, args, assoc_constraints)?;
                Ok(Some(ast::mk::trait_bound(ast::TraitBoundModifiers::NONE, path)))
            })?;

            let auto_traits = predicates.auto_traits()
                .map(|def_id| -> Result<_, Self::Error> {
                    let def_path = self.print_def_path(def_id, &[])?;
                    Ok(ast::mk::trait_bound(ast::TraitBoundModifiers::NONE, def_path))
                })
                .try_collect::<Vec<_>>()?;

            let bounds = principal.into_iter()
                .chain(auto_traits.into_iter())
                .collect();

            Ok(ast::mk::ty(sp, ast::TyKind::TraitObject(bounds, ast::TraitObjectSyntax::Dyn)))
        }

        fn print_ty(&mut self, ty: Ty<'tcx>) -> Result<Self::Type, Self::Error> {
            let sp = self.sp;

            match *ty.kind() {
                ty::TyKind::Bool => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::bool, sp))),
                ty::TyKind::Char => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::char, sp))),
                ty::TyKind::Int(ty::IntTy::I8) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::i8, sp))),
                ty::TyKind::Int(ty::IntTy::I16) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::i16, sp))),
                ty::TyKind::Int(ty::IntTy::I32) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::i32, sp))),
                ty::TyKind::Int(ty::IntTy::I64) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::i64, sp))),
                ty::TyKind::Int(ty::IntTy::I128) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::i128, sp))),
                ty::TyKind::Int(ty::IntTy::Isize) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::isize, sp))),
                ty::TyKind::Uint(ty::UintTy::U8) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::u8, sp))),
                ty::TyKind::Uint(ty::UintTy::U16) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::u16, sp))),
                ty::TyKind::Uint(ty::UintTy::U32) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::u32, sp))),
                ty::TyKind::Uint(ty::UintTy::U64) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::u64, sp))),
                ty::TyKind::Uint(ty::UintTy::U128) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::u128, sp))),
                ty::TyKind::Uint(ty::UintTy::Usize) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::usize, sp))),
                ty::TyKind::Float(ty::FloatTy::F16) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::f16, sp))),
                ty::TyKind::Float(ty::FloatTy::F32) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::f32, sp))),
                ty::TyKind::Float(ty::FloatTy::F64) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::f64, sp))),
                ty::TyKind::Float(ty::FloatTy::F128) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::f128, sp))),
                ty::TyKind::Str => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::str, sp))),
                ty::TyKind::Never => Ok(ast::mk::ty(sp, ast::TyKind::Never)),

                ty::TyKind::RawPtr(ty, mutbl) => {
                    let inner_ty = self.print_ty(ty)?;
                    Ok(ast::mk::ty_ptr(sp, inner_ty, mutbl))
                }
                ty::TyKind::Ref(region, ty, mutbl) => {
                    let inner_ty = self.print_ty(ty)?;
                    let lifetime = self.print_region(region)?;
                    Ok(ast::mk::ty_rptr(sp, inner_ty, lifetime, mutbl))
                }
                ty::TyKind::Tuple(tys) => {
                    let inner_tys = tys.iter().map(|ty| self.print_ty(ty)).try_collect()?;
                    Ok(ast::mk::ty_tuple(sp, inner_tys))
                }
                ty::TyKind::Slice(ty) => {
                    let inner_ty = self.print_ty(ty)?;
                    Ok(ast::mk::ty_slice(sp, inner_ty))
                }
                ty::TyKind::Array(ty, size) => {
                    let inner_ty = self.print_ty(ty)?;
                    let size_const = self.print_const(size)?;
                    Ok(ast::mk::ty_array(sp, inner_ty, size_const))
                }

                ty::TyKind::Adt(def, args) => {
                    let def_path = self.print_def_path(def.did(), args)?;
                    Ok(ast::mk::ty_path(None, def_path))
                }
                ty::TyKind::Foreign(def_id) => {
                    // TODO
                    let def_path = self.print_def_path(def_id, &[])?;
                    Ok(ast::mk::ty_path(None, def_path))
                }
                ty::TyKind::Dynamic(predicates, region, dyn_kind) => {
                    let mut dyn_existential = self.print_dyn_existential(predicates)?;
                    if let Some(lifetime) = self.print_region(region)? {
                        let ast::TyKind::TraitObject(bounds, syntax) = &mut dyn_existential.kind else { unreachable!() };
                        bounds.push(ast::mk::lifetime_bound(lifetime));
                        *syntax = match dyn_kind {
                            ty::DynKind::Dyn => ast::TraitObjectSyntax::Dyn,
                            ty::DynKind::DynStar => ast::TraitObjectSyntax::DynStar,
                        };
                    }
                    Ok(dyn_existential)
                }
                ty::TyKind::Alias(alias_kind, alias_ty) => {
                    match alias_kind {
                        ty::AliasTyKind::Opaque => {
                            match self.opaque_ty_handling {
                                OpaqueTyHandling::Infer => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                                OpaqueTyHandling::Keep => {
                                    let def_path = self.print_def_path(alias_ty.def_id, alias_ty.args)?;
                                    Ok(ast::mk::ty(sp, ast::TyKind::ImplTrait(ast::DUMMY_NODE_ID, vec![
                                        ast::mk::trait_bound(ast::TraitBoundModifiers::NONE, def_path)
                                    ])))
                                }
                                OpaqueTyHandling::Resolve => {
                                    let ty = self.tcx.type_of(alias_ty.def_id).instantiate_identity();
                                    self.print_ty(ty)
                                }
                            }
                        }
                        ty::AliasTyKind::Projection => {
                            let def_path = self.print_def_path(alias_ty.def_id, alias_ty.args)?;

                            let self_ty = self.print_ty(alias_ty.self_ty())?;
                            let qself = P(ast::QSelf {
                                ty: self_ty,
                                path_span: DUMMY_SP,
                                position: def_path.segments.len() - 1,
                            });

                            Ok(ast::mk::ty_path(Some(qself), def_path))
                        }
                        ty::AliasTyKind::Inherent | ty::AliasTyKind::Free => {
                            let def_path = self.print_def_path(alias_ty.def_id, alias_ty.args)?;
                            Ok(ast::mk::ty_path(None, def_path))
                        }
                    }
                }
                ty::TyKind::Param(param_ty) => {
                    // Avoid naming synthetic generic params from `impl Trait` function parameters.
                    if param_ty.name.as_str().starts_with("impl ") {
                        return Ok(ast::mk::ty(sp, ast::TyKind::Infer));
                    }

                    let mut ident = Ident::new(param_ty.name, sp);
                    if self.sanitize_macro_expns {
                        'sanitize: {
                            let Some(scope) = self.scope else { break 'sanitize; };
                            if ident.name == kw::SelfUpper { break 'sanitize; }
                            let def_ident_span = param_ty.span_from_generics(self.tcx, scope);
                            hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                        }
                    }
                    Ok(ast::mk::ty_ident(sp, None, ident))
                }

                ty::TyKind::FnPtr(fn_sig_tys, fn_header) => {
                    let fn_sig_tys = fn_sig_tys.skip_binder();

                    let input_tys_ast = fn_sig_tys.inputs().iter().copied().map(|ty| self.print_ty(ty)).try_collect::<Vec<_>>()?;
                    let output_ty_ast = self.print_ty(fn_sig_tys.output())?;

                    let input_params = input_tys_ast.into_iter().map(|ty| ast::mk::param(sp, ast::mk::pat_wild(sp), ty)).collect();
                    Ok(ast::mk::ty(sp, ast::TyKind::BareFn(P(ast::BareFnTy {
                        safety: match fn_header.safety {
                            hir::Safety::Safe => ast::Safety::Default,
                            hir::Safety::Unsafe => ast::Safety::Unsafe(sp),
                        },
                        ext: ast::Extern::Explicit(ast::StrLit {
                            span: sp,
                            style: ast::StrStyle::Cooked,
                            symbol: Symbol::intern(fn_header.abi.name()),
                            symbol_unescaped: Symbol::intern(fn_header.abi.name()),
                            suffix: None,
                        }, DUMMY_SP),
                        generic_params: ThinVec::new(),
                        decl: ast::mk::fn_decl(input_params, ast::FnRetTy::Ty(output_ty_ast)),
                        decl_span: DUMMY_SP,
                    }))))
                }

                // NOTE: These types cannot be represented directly in the AST, so we must replace them with inference holes.
                ty::TyKind::FnDef(_, _) => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                ty::TyKind::Closure(_, _) => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                ty::TyKind::Coroutine(_, _) => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                ty::TyKind::CoroutineClosure(_, _) => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                ty::TyKind::CoroutineWitness(_, _) => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),

                ty::TyKind::Bound(_, _) => Err("encountered bound type variable".to_owned()),
                ty::TyKind::UnsafeBinder(_) => Err("encountered unsafe binder".to_owned()),
                ty::TyKind::Infer(_) => Err("encountered type variable".to_owned()),
                ty::TyKind::Pat(_, _) => Err("encountered pat".to_owned()),

                ty::TyKind::Placeholder(_) => Err("encountered placeholder type".to_owned()),
                ty::TyKind::Error(_) => Err("encountered type error".to_owned()),
            }
        }
    }

    pub fn ast_repr<'tcx>(
        tcx: TyCtxt<'tcx>,
        crate_res: &res::CrateResolutions<'tcx>,
        def_res: &ast_lowering::DefResolutions,
        scope: Option<hir::DefId>,
        sp: Span,
        ty: Ty<'tcx>,
        def_path_handling: DefPathHandling,
        opaque_ty_handling: OpaqueTyHandling,
        sanitize_macro_expns: bool,
        binding_item_def_id: hir::DefId,
    ) -> Option<P<ast::Ty>> {
        let mut printer = AstTyPrinter {
            tcx,
            crate_res,
            def_res,
            scope,
            sp,
            def_path_handling,
            opaque_ty_handling,
            sanitize_macro_expns,
            binding_item_def_id,
        };
        printer.print_ty(ty).ok()
    }
}
