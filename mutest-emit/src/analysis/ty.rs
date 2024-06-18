pub use rustc_middle::ty::*;

use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::ty;
use rustc_trait_selection::infer::InferCtxtExt;

use crate::analysis::hir;
use crate::codegen::symbols::{DUMMY_SP, Ident, Symbol};

pub fn impls_trait_with_env<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: Vec<ty::GenericArg<'tcx>>) -> bool {
    let ty = tcx.erase_regions(ty);
    if ty.has_escaping_bound_vars() { return false; }

    let infcx = tcx.infer_ctxt().build();
    infcx.type_implements_trait(trait_def_id, tcx.mk_args_trait(ty, args), param_env).must_apply_modulo_regions()
}

pub fn impls_trait<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: Vec<ty::GenericArg<'tcx>>) -> bool {
    impls_trait_with_env(tcx, ty::ParamEnv::empty(), ty, trait_def_id, args)
}

pub fn impl_assoc_ty<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, caller_def_id: hir::LocalDefId, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: Vec<ty::GenericArg<'tcx>>, assoc_ty: Symbol) -> Option<Ty<'tcx>> {
    tcx.associated_items(trait_def_id)
        .find_by_name_and_kind(tcx, Ident::new(assoc_ty, DUMMY_SP), ty::AssocKind::Type, trait_def_id)
        .and_then(|assoc_item| {
            let args = tcx.with_opt_host_effect_param(caller_def_id, trait_def_id, tcx.mk_args_trait(ty, args));
            let proj = Ty::new_projection(tcx, assoc_item.def_id, args);
            tcx.try_normalize_erasing_regions(param_env, proj).ok()
        })
}

pub use print::ast_repr;

pub mod print {
    use std::iter;

    use rustc_middle::mir;
    use rustc_middle::ty::{self, Ty, TyCtxt};
    use rustc_session::cstore::{ExternCrate, ExternCrateSource};
    use thin_vec::ThinVec;

    use crate::analysis::hir::{self, LOCAL_CRATE};
    use crate::analysis::res;
    use crate::codegen::ast::{self, P};
    use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym, kw};

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

        fn path_crate(&mut self, cnum: hir::CrateNum) -> Result<Self::Path, Self::Error>;
        fn path_append(&mut self, path: Self::Path, disambiguated_data: &hir::DisambiguatedDefPathData) -> Result<Self::Path, Self::Error>;
        fn path_generic_args(
            &mut self,
            path: Self::Path,
            args: &[ty::GenericArg<'tcx>],
            assoc_constraints: impl Iterator<Item = ty::ExistentialProjection<'tcx>>,
        ) -> Result<Self::Path, Self::Error>;

        fn print_res_def_path(&mut self, def_path: res::DefPath) -> Result<Self::Path, Self::Error>;

        fn try_print_visible_def_path(&mut self, def_id: hir::DefId, scoped_item_paths: ScopedItemPaths) -> Result<Option<Self::Path>, Self::Error> {
            fn try_print_visible_def_path_impl<'tcx, T: Printer<'tcx> + ?Sized>(
                printer: &mut T,
                def_id: hir::DefId,
                scoped_item_paths: ScopedItemPaths,
                callers: &mut Vec<hir::DefId>,
            ) -> Result<Option<T::Path>, T::Error> {
                if let Some(cnum) = def_id.as_crate_root() {
                    if cnum == LOCAL_CRATE {
                        return Ok(Some(printer.path_crate(cnum)?));
                    }

                    match printer.tcx().extern_crate(def_id) {
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

                let visible_def_paths = res::visible_def_paths(printer.tcx(), def_id, None);
                if let Some(visible_def_path) = visible_def_paths.into_iter().next() {
                    return Some(printer.print_res_def_path(visible_def_path)).transpose();
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
                let Some(path) = try_print_visible_def_path_impl(printer, visible_parent, scoped_item_paths, callers)? else { return Ok(None); };
                callers.pop();

                let disambiguated_data = hir::DisambiguatedDefPathData { data, disambiguator: 0 };
                let path = printer.path_append(path, &disambiguated_data)?;
                Ok(Some(path))
            }

            let mut callers = vec![];
            try_print_visible_def_path_impl(self, def_id, scoped_item_paths, &mut callers)
        }

        fn print_def_path(&mut self, def_id: hir::DefId, args: &'tcx [ty::GenericArg<'tcx>]) -> Result<Self::Path, Self::Error>;

        fn print_region(&mut self, region: ty::Region<'_>) -> Result<Self::Region, Self::Error>;
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
    struct AstTyPrinter<'tcx> {
        tcx: TyCtxt<'tcx>,
        sp: Span,
        def_path_handling: DefPathHandling,
        opaque_ty_handling: OpaqueTyHandling,
    }

    impl<'tcx> Printer<'tcx> for AstTyPrinter<'tcx> {
        type Error = String;

        type Type = P<ast::Ty>;
        type DynExistential = P<ast::Ty>;
        type Const = ast::AnonConst;
        type Region = Option<ast::Lifetime>;
        type Path = ast::Path;

        fn tcx(&self) -> TyCtxt<'tcx> {
            self.tcx
        }

        fn path_crate(&mut self, cnum: hir::CrateNum) -> Result<Self::Path, Self::Error> {
            match cnum {
                LOCAL_CRATE => Ok(ast::mk::path_ident(self.sp, Ident::new(kw::Crate, self.sp))),
                _ => {
                    let crate_name = self.tcx.crate_name(cnum);
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
            let mut errors = vec![];

            let mut args_ast = args.iter()
                .map(|arg: &ty::GenericArg<'tcx>| -> Result<Option<ast::AngleBracketedArg>, Self::Error> {
                    match arg.unpack() {
                        ty::GenericArgKind::Type(ty) => {
                            let ty_ast = self.print_ty(ty)?;
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
                    let name = self.tcx.associated_item(assoc_constraint.def_id).name;

                    let term_ast = match assoc_constraint.term.unpack() {
                        ty::TermKind::Ty(ty) => ast::Term::Ty(self.print_ty(ty)?),
                        ty::TermKind::Const(ct) => ast::Term::Const(self.print_const(ct)?),
                    };

                    Ok(ast::AngleBracketedArg::Constraint(ast::AssocConstraint {
                        id: ast::DUMMY_NODE_ID,
                        ident: Ident::new(name, self.sp),
                        kind: ast::AssocConstraintKind::Equality { term: term_ast },
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

        fn print_res_def_path(&mut self, def_path: res::DefPath) -> Result<Self::Path, Self::Error> {
            Ok(def_path.ast_path())
        }

        fn print_def_path(&mut self, def_id: hir::DefId, args: &'tcx [ty::GenericArg<'tcx>]) -> Result<Self::Path, Self::Error> {
            if let DefPathHandling::PreferVisible(scoped_item_paths) | DefPathHandling::ForceVisible(scoped_item_paths) = self.def_path_handling {
                if let Some(path) = self.try_print_visible_def_path(def_id, scoped_item_paths)? {
                    if args.is_empty() { return Ok(path); }
                    return self.path_generic_args(path, args, iter::empty());
                }

                if let DefPathHandling::ForceVisible(_) = self.def_path_handling {
                    return Err("cannot find visible path for definition".to_owned());
                }
            }

            // FIXME
            Err("encountered definition with no visible path".to_owned())
        }

        fn print_region(&mut self, region: ty::Region<'_>) -> Result<Self::Region, Self::Error> {
            let sp = self.sp;

            match *region {
                ty::RegionKind::ReStatic => Ok(Some(ast::mk::lifetime(sp, Ident::new(kw::StaticLifetime, sp)))),

                ty::RegionKind::ReEarlyParam(region) => {
                    if region.name == kw::Empty { return Ok(None); }
                    Ok(Some(ast::mk::lifetime(sp, Ident::new(region.name, sp))))
                }

                | ty::RegionKind::ReBound(_, ty::BoundRegion { kind: region, .. })
                | ty::RegionKind::ReLateParam(ty::LateParamRegion { bound_region: region, .. })
                | ty::RegionKind::RePlaceholder(ty::Placeholder { bound: ty::BoundRegion { kind: region, .. }, .. })
                => {
                    let ty::BoundRegionKind::BrNamed(_, region_name) = region else { return Ok(None); };
                    if region_name == kw::Empty || region_name == kw::UnderscoreLifetime { return Ok(None); }
                    Ok(Some(ast::mk::lifetime(sp, Ident::new(region_name, sp))))
                }

                ty::RegionKind::ReVar(_) | ty::RegionKind::ReErased => Ok(None),

                ty::RegionKind::ReError(_) => Err("encountered region error".to_owned()),
            }
        }

        fn print_const(&mut self, ct: ty::Const<'tcx>) -> Result<Self::Const, Self::Error> {
            let sp = self.sp;

            let valtree = ct.eval(self.tcx, ty::ParamEnv::reveal_all(), DUMMY_SP).map_err(|_| format!("encountered invalid const"))?;
            let val = self.tcx.valtree_to_const_val((ct.ty(), valtree));

            match val {
                mir::ConstValue::Scalar(scalar) => {
                    let lit_expr = match ct.ty().kind() {
                        ty::TyKind::Bool => {
                            scalar.to_bool().map(|v| ast::mk::expr_bool(sp, v))
                                .map_err(|e| format!("encountered invalid bool const: {e:?}"))
                        }
                        ty::TyKind::Char => {
                            scalar.to_char().map(|v| ast::mk::expr_lit(sp, ast::token::LitKind::Char, Symbol::intern(&v.to_string()), None))
                                .map_err(|e| format!("encountered invalid char const: {e:?}"))
                        }
                        ty::TyKind::Int(ty::IntTy::I8) => {
                            scalar.to_i8().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i8))
                                .map_err(|e| format!("encountered invalid i8 const: {e:?}"))
                        }
                        ty::TyKind::Int(ty::IntTy::I16) => {
                            scalar.to_i16().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i16))
                                .map_err(|e| format!("encountered invalid i16 const: {e:?}"))
                        }
                        ty::TyKind::Int(ty::IntTy::I32) => {
                            scalar.to_i32().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i32))
                                .map_err(|e| format!("encountered invalid i32 const: {e:?}"))
                        }
                        ty::TyKind::Int(ty::IntTy::I64) => {
                            scalar.to_i64().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i64))
                                .map_err(|e| format!("encountered invalid i64 const: {e:?}"))
                        }
                        ty::TyKind::Int(ty::IntTy::I128) => {
                            scalar.to_i128().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::i128))
                                .map_err(|e| format!("encountered invalid i128 const: {e:?}"))
                        }
                        ty::TyKind::Int(ty::IntTy::Isize) => {
                            scalar.to_target_isize(&self.tcx).map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::isize))
                                .map_err(|e| format!("encountered invalid isize const: {e:?}"))
                        }
                        ty::TyKind::Uint(ty::UintTy::U8) => {
                            scalar.to_u8().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u8))
                                .map_err(|e| format!("encountered invalid u8 const: {e:?}"))
                        }
                        ty::TyKind::Uint(ty::UintTy::U16) => {
                            scalar.to_u16().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u16))
                                .map_err(|e| format!("encountered invalid u16 const: {e:?}"))
                        }
                        ty::TyKind::Uint(ty::UintTy::U32) => {
                            scalar.to_u32().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u32))
                                .map_err(|e| format!("encountered invalid u32 const: {e:?}"))
                        }
                        ty::TyKind::Uint(ty::UintTy::U64) => {
                            scalar.to_u64().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u64))
                                .map_err(|e| format!("encountered invalid u64 const: {e:?}"))
                        }
                        ty::TyKind::Uint(ty::UintTy::U128) => {
                            scalar.to_u128().map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::u128))
                                .map_err(|e| format!("encountered invalid u128 const: {e:?}"))
                        }
                        ty::TyKind::Uint(ty::UintTy::Usize) => {
                            scalar.to_target_usize(&self.tcx).map(|v| ast::mk::expr_int_exact(sp, v as isize, sym::usize))
                                .map_err(|e| format!("encountered invalid usize const: {e:?}"))
                        }
                        ty::TyKind::Float(ty::FloatTy::F16) => {
                            scalar.to_f16()
                                .map(|v| {
                                    let v = f16::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u16);
                                    ast::mk::expr_float_exact(sp, v as f64, sym::f16)
                                })
                                .map_err(|e| format!("encountered invalid f16 const: {e:?}"))
                        }
                        ty::TyKind::Float(ty::FloatTy::F32) => {
                            scalar.to_f32()
                                .map(|v| {
                                    let v = f32::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u32);
                                    ast::mk::expr_float_exact(sp, v as f64, sym::f32)
                                })
                                .map_err(|e| format!("encountered invalid f32 const: {e:?}"))
                        }
                        ty::TyKind::Float(ty::FloatTy::F64) => {
                            scalar.to_f64()
                                .map(|v| {
                                    let v = f64::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u64);
                                    ast::mk::expr_float_exact(sp, v, sym::f64)
                                })
                                .map_err(|e| format!("encountered invalid f64 const: {e:?}"))
                        }
                        ty::TyKind::Float(ty::FloatTy::F128) => {
                            scalar.to_f16()
                                .map(|v| {
                                    let v = f128::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u128);
                                    ast::mk::expr_float_exact(sp, v as f64, sym::f128)
                                })
                                .map_err(|e| format!("encountered invalid f16 const: {e:?}"))
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
                    let output_ty = projection.skip_binder().term.ty();

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
                                    ], None)))
                                }
                                OpaqueTyHandling::Resolve => {
                                    let ty = self.tcx.type_of(alias_ty.def_id).instantiate_identity();
                                    self.print_ty(ty)
                                }
                            }
                        }
                        ty::AliasTyKind::Projection | ty::AliasTyKind::Inherent | ty::AliasTyKind::Weak => {
                            let def_path = self.print_def_path(alias_ty.def_id, alias_ty.args)?;
                            Ok(ast::mk::ty_path(None, def_path))
                        }
                    }
                }
                ty::TyKind::Param(param_ty) => {
                    Ok(ast::mk::ty_ident(sp, None, Ident::new(param_ty.name, sp)))
                }

                ty::TyKind::FnPtr(sig) => {
                    let sig = sig.skip_binder();

                    let input_tys_ast = sig.inputs().iter().copied().map(|ty| self.print_ty(ty)).try_collect::<Vec<_>>()?;
                    let output_ty_ast = self.print_ty(sig.output())?;

                    let input_params = input_tys_ast.into_iter().map(|ty| ast::mk::param(sp, ast::mk::pat_wild(sp), ty)).collect();
                    Ok(ast::mk::ty(sp, ast::TyKind::BareFn(P(ast::BareFnTy {
                        unsafety: match sig.unsafety {
                            hir::Unsafety::Normal => ast::Unsafe::No,
                            hir::Unsafety::Unsafe => ast::Unsafe::Yes(sp),
                        },
                        ext: ast::Extern::Explicit(ast::StrLit {
                            span: sp,
                            style: ast::StrStyle::Cooked,
                            symbol: Symbol::intern(sig.abi.name()),
                            symbol_unescaped: Symbol::intern(sig.abi.name()),
                            suffix: None,
                        }, DUMMY_SP),
                        generic_params: ThinVec::new(),
                        decl: ast::mk::fn_decl(input_params, ast::FnRetTy::Ty(output_ty_ast)),
                        decl_span: DUMMY_SP,
                    }))))
                }

                ty::TyKind::Pat(_, _) => Err("encountered pat".to_owned()),
                ty::TyKind::Bound(_, _) => Err("encountered bound type variable".to_owned()),
                ty::TyKind::Infer(_) => Err("encountered type variable".to_owned()),

                ty::TyKind::FnDef(_, _) => Err("encountered anonymous function declaration type".to_owned()),
                ty::TyKind::Closure(_, _) => Err("encountered anonymous closure type".to_owned()),
                ty::TyKind::Coroutine(_, _) => Err("encountered anonymous coroutine type".to_owned()),
                ty::TyKind::CoroutineClosure(_, _) => Err("encountered anonymous coroutine closure type".to_owned()),
                ty::TyKind::CoroutineWitness(_, _) => Err("encountered anonymous coroutine storage type".to_owned()),

                ty::TyKind::Placeholder(_) => Err("encountered placeholder type".to_owned()),
                ty::TyKind::Error(_) => Err("encountered type error".to_owned()),
            }
        }
    }

    pub fn ast_repr<'tcx>(tcx: TyCtxt<'tcx>, sp: Span, ty: Ty<'tcx>, def_path_handling: DefPathHandling, opaque_ty_handling: OpaqueTyHandling) -> Option<P<ast::Ty>> {
        let mut printer = AstTyPrinter {
            tcx,
            sp,
            def_path_handling,
            opaque_ty_handling,
        };
        printer.print_ty(ty).ok()
    }
}
