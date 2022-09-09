pub use rustc_middle::ty::*;

use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::ty;
use rustc_middle::ty::print::Printer;
use rustc_session::cstore::{ExternCrate, ExternCrateSource};
use rustc_target::spec::abi::Abi;
use rustc_trait_selection::infer::InferCtxtExt;

use crate::analysis::hir::{self, LOCAL_CRATE};
use crate::codegen::ast::{self, P};
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym, kw};

pub fn impls_trait_with_env<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: &[ty::GenericArg<'tcx>]) -> bool {
    let ty = tcx.erase_regions(ty);
    if ty.has_escaping_bound_vars() { return false; }

    let args = tcx.mk_substs(args.iter());

    tcx.infer_ctxt().enter(|infcx| {
        infcx.type_implements_trait(trait_def_id, ty, args, param_env).must_apply_modulo_regions()
    })
}

pub fn impls_trait<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: &[ty::GenericArg<'tcx>]) -> bool {
    impls_trait_with_env(tcx, ty::ParamEnv::empty(), ty, trait_def_id, args)
}

pub fn impl_assoc_ty<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: &[ty::GenericArg<'tcx>], assoc_ty: Symbol) -> Option<Ty<'tcx>> {
    tcx.associated_items(trait_def_id)
        .find_by_name_and_kind(tcx, Ident::new(assoc_ty, DUMMY_SP), ty::AssocKind::Type, trait_def_id)
        .and_then(|assoc_item| {
            let proj = tcx.mk_projection(assoc_item.def_id, tcx.mk_substs_trait(ty, args));
            tcx.try_normalize_erasing_regions(param_env, proj).ok()
        })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopedItemPaths {
    FullyQualified,
    Trimmed,
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

impl<'tcx> ty::print::Printer<'tcx> for AstTyPrinter<'tcx> {
    type Error = String;

    type Type = P<ast::Ty>;
    type DynExistential = P<ast::Ty>;
    type Const = ast::AnonConst;
    type Region = Option<ast::Lifetime>;
    type Path = ast::Path;

    fn tcx<'a>(&'a self) -> TyCtxt<'tcx> {
        self.tcx
    }

    fn print_type(self, ty: Ty<'tcx>) -> Result<Self::Type, Self::Error> {
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
            ty::TyKind::Float(ty::FloatTy::F32) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::f32, sp))),
            ty::TyKind::Float(ty::FloatTy::F64) => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::f64, sp))),
            ty::TyKind::Str => Ok(ast::mk::ty_ident(sp, None, Ident::new(sym::str, sp))),
            ty::TyKind::Never => Ok(ast::mk::ty(sp, ast::TyKind::Never)),

            ty::TyKind::Param(param) => {
                let is_opaque_ty = param.name.as_str().starts_with("impl ");

                match (is_opaque_ty, self.opaque_ty_handling) {
                    (false, _) | (_, OpaqueTyHandling::Keep) => {
                        Ok(ast::mk::ty_ident(sp, None, Ident::new(param.name, sp)))
                    }
                    (true, OpaqueTyHandling::Infer) => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                    (true, OpaqueTyHandling::Resolve) => Err("opaque type parameter cannot be resolved".to_owned()),
                }
            }
            ty::TyKind::Bound(_, _) => Err("encountered bound type variable".to_owned()),
            ty::TyKind::Infer(_) => Err("encountered type variable".to_owned()),

            ty::TyKind::RawPtr(ty_and_mut) => {
                let inner_ty = self.print_type(ty_and_mut.ty)?;
                Ok(ast::mk::ty_ptr(sp, inner_ty, ty_and_mut.mutbl))
            }
            ty::TyKind::Ref(region, ty, mutbl) => {
                let inner_ty = self.print_type(ty)?;
                let lifetime = self.print_region(region)?;
                Ok(ast::mk::ty_rptr(sp, inner_ty, lifetime, mutbl))
            }
            ty::TyKind::Array(ty, size) => {
                let inner_ty = self.print_type(ty)?;
                let size_const = self.print_const(size)?;
                Ok(ast::mk::ty_array(sp, inner_ty, size_const))
            }
            ty::TyKind::Slice(ty) => {
                let inner_ty = self.print_type(ty)?;
                Ok(ast::mk::ty_slice(sp, inner_ty))
            }
            ty::TyKind::Tuple(tys) => {
                let inner_tys = tys.iter().map(|ty| self.print_type(ty)).try_collect()?;
                Ok(ast::mk::ty_tuple(sp, inner_tys))
            }

            ty::TyKind::Adt(def, substs) => {
                let def_path = self.print_def_path(def.did(), substs)?;
                Ok(ast::mk::ty_path(None, def_path))
            }
            ty::TyKind::Foreign(def_id) => {
                let def_path = self.print_def_path(def_id, &[])?;
                Ok(ast::mk::ty_path(None, def_path))
            }
            ty::TyKind::Dynamic(predicates, region) => {
                let mut dyn_existential = self.print_dyn_existential(predicates)?;
                if let Some(lifetime) = self.print_region(region)? {
                    let ast::TyKind::TraitObject(bounds, _) = &mut dyn_existential.kind else { unreachable!() };
                    bounds.push(ast::mk::lifetime_bound(lifetime))
                }
                Ok(dyn_existential)
            }
            ty::TyKind::Opaque(def_id, substs) => {
                match self.opaque_ty_handling {
                    OpaqueTyHandling::Infer => Ok(ast::mk::ty(sp, ast::TyKind::Infer)),
                    OpaqueTyHandling::Keep => {
                        let def_path = self.print_def_path(def_id, substs)?;
                        Ok(ast::mk::ty(sp, ast::TyKind::ImplTrait(ast::DUMMY_NODE_ID, vec![
                            ast::mk::trait_bound(ast::TraitBoundModifier::None, def_path)
                        ])))
                    }
                    OpaqueTyHandling::Resolve => {
                        let ty = self.tcx.type_of(def_id);
                        self.print_type(ty)
                    }
                }
            }
            ty::TyKind::Projection(projection) => {
                let def_path = self.print_def_path(projection.item_def_id, projection.substs)?;
                Ok(ast::mk::ty_path(None, def_path))
            }
            ty::TyKind::FnPtr(sig) => {
                let sig = sig.skip_binder();

                let input_tys_ast = sig.inputs().iter().copied().map(|ty| self.print_type(ty)).try_collect::<Vec<_>>()?;
                let output_ty_ast = self.print_type(sig.output())?;

                let input_params = input_tys_ast.into_iter().map(|ty| ast::mk::param(sp, ast::mk::pat_wild(sp), ty)).collect();
                Ok(ast::mk::ty(sp, ast::TyKind::BareFn(P(ast::BareFnTy {
                    unsafety: match sig.unsafety {
                        hir::Unsafety::Normal => ast::Unsafe::No,
                        hir::Unsafety::Unsafe => ast::Unsafe::Yes(sp),
                    },
                    ext: match sig.abi {
                        Abi::Rust => ast::Extern::Implicit,
                        _ => ast::Extern::Explicit(ast::StrLit {
                            span: sp,
                            style: ast::StrStyle::Cooked,
                            symbol: Symbol::intern(sig.abi.name()),
                            symbol_unescaped: Symbol::intern(sig.abi.name()),
                            suffix: None,
                        }),
                    },
                    generic_params: vec![],
                    decl: ast::mk::fn_decl(input_params, ast::FnRetTy::Ty(output_ty_ast)),
                    decl_span: DUMMY_SP,
                }))))
            }

            ty::TyKind::FnDef(_, _) => Err("encountered anonymous function declaration type".to_owned()),
            ty::TyKind::Closure(_, _) => Err("encountered anonymous closure type".to_owned()),
            ty::TyKind::Generator(_, _, _) => Err("encountered anonymous generator type".to_owned()),
            ty::TyKind::GeneratorWitness(_) => Err("encountered anonymous generator storage type".to_owned()),

            ty::TyKind::Placeholder(_) => Err("encountered placeholder type".to_owned()),
            ty::TyKind::Error(_) => Err("encountered type error".to_owned()),
        }
    }

    fn print_dyn_existential(
        self,
        predicates: &'tcx ty::List<ty::Binder<'tcx, ty::ExistentialPredicate<'tcx>>>,
    ) -> Result<Self::DynExistential, Self::Error> {
        let sp = self.sp;

        let principal = predicates.principal().map_or(Ok(None), |principal| -> Result<_, Self::Error> {
            let principal = principal.skip_binder();

            let def_path = self.print_def_path(principal.def_id, &[])?;

            // Fn(...) -> ...
            if let Some(_) = self.tcx.fn_trait_kind_from_lang_item(principal.def_id)
                && let ty::Tuple(input_tys) = principal.substs.type_at(0).kind()
                && let mut projections = predicates.projection_bounds()
                && let (Some(projection), None) = (projections.next(), projections.next())
            {
                let output_ty = projection.skip_binder().term.ty();

                let input_tys_ast = input_tys.iter().map(|ty| self.print_type(ty)).try_collect()?;
                let output_ty_ast = output_ty.map_or(Result::<_, Self::Error>::Ok(None), |ty| Ok(Some(self.print_type(ty)?)))?;
                let args = ast::mk::parenthesized_args(sp, input_tys_ast, output_ty_ast);
                let path = ast::mk::pathx_raw(sp, def_path, vec![], Some(args));
                return Ok(Some(ast::mk::trait_bound(ast::TraitBoundModifier::None, path)));
            }

            let dummy_self_ty = self.tcx.mk_ty_infer(ty::FreshTy(0));
            let principal = principal.with_self_ty(self.tcx, dummy_self_ty);

            let args = self.tcx.generics_of(principal.def_id).own_substs_no_defaults(self.tcx, principal.substs);
            let path = self.path_generic_args(|_| Ok(def_path), args)?;
            Ok(Some(ast::mk::trait_bound(ast::TraitBoundModifier::None, path)))
        })?;

        let auto_traits = predicates.auto_traits()
            .map(|def_id| -> Result<_, Self::Error> {
                let def_path = self.print_def_path(def_id, &[])?;
                Ok(ast::mk::trait_bound(ast::TraitBoundModifier::None, def_path))
            })
            .try_collect::<Vec<_>>()?;

        let bounds = principal.into_iter()
            .chain(auto_traits.into_iter())
            .collect();

        Ok(ast::mk::ty(sp, ast::TyKind::TraitObject(bounds, ast::TraitObjectSyntax::Dyn)))
    }

    fn print_const(self, ct: ty::Const<'tcx>) -> Result<Self::Const, Self::Error> {
        let sp = self.sp;

        match ct.val() {
            ty::ConstKind::Value(rustc_const_eval::interpret::ConstValue::Scalar(scalar)) => {
                match ct.ty().kind() {
                    ty::TyKind::Bool => {
                        scalar.to_bool().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Bool(v)).into_inner().kind))
                            .map_err(|e| format!("encountered invalid bool const: {e}"))
                    }
                    ty::TyKind::Char => {
                        scalar.to_char().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Char(v)).into_inner().kind))
                            .map_err(|e| format!("encountered invalid char const: {e}"))
                    }
                    ty::TyKind::Int(ty::IntTy::I8) => {
                        scalar.to_i8().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I8))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid i8 const: {e}"))
                    }
                    ty::TyKind::Int(ty::IntTy::I16) => {
                        scalar.to_i16().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I16))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid i16 const: {e}"))
                    }
                    ty::TyKind::Int(ty::IntTy::I32) => {
                        scalar.to_i32().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I32))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid i32 const: {e}"))
                    }
                    ty::TyKind::Int(ty::IntTy::I64) => {
                        scalar.to_i64().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I64))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid i64 const: {e}"))
                    }
                    ty::TyKind::Int(ty::IntTy::I128) => {
                        scalar.to_i128().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I128))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid i128 const: {e}"))
                    }
                    ty::TyKind::Int(ty::IntTy::Isize) => {
                        scalar.to_machine_isize(&self.tcx).map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::Isize))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid isize const: {e}"))
                    }
                    ty::TyKind::Uint(ty::UintTy::U8) => {
                        scalar.to_u8().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U8))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid u8 const: {e}"))
                    }
                    ty::TyKind::Uint(ty::UintTy::U16) => {
                        scalar.to_u16().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U16))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid u16 const: {e}"))
                    }
                    ty::TyKind::Uint(ty::UintTy::U32) => {
                        scalar.to_u32().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U32))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid u32 const: {e}"))
                    }
                    ty::TyKind::Uint(ty::UintTy::U64) => {
                        scalar.to_u64().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U64))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid u64 const: {e}"))
                    }
                    ty::TyKind::Uint(ty::UintTy::U128) => {
                        scalar.to_u128().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U128))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid u128 const: {e}"))
                    }
                    ty::TyKind::Uint(ty::UintTy::Usize) => {
                        scalar.to_machine_usize(&self.tcx).map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::Usize))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid usize const: {e}"))
                    }
                    ty::TyKind::Float(ty::FloatTy::F32) => {
                        scalar.to_f32().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Float(Symbol::intern(&v.to_string()), ast::LitFloatType::Suffixed(ast::FloatTy::F32))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid f32 const: {e}"))
                    }
                    ty::TyKind::Float(ty::FloatTy::F64) => {
                        scalar.to_f64().map(|v| ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Float(Symbol::intern(&v.to_string()), ast::LitFloatType::Suffixed(ast::FloatTy::F64))).into_inner().kind))
                            .map_err(|e| format!("encountered invalid f64 const: {e}"))
                    }
                    _ => Err("encountered unknown constant scalar value".to_owned())
                }
            }
            ty::ConstKind::Value(rustc_const_eval::interpret::ConstValue::Slice { .. }) => Err("encountered slice constant value".to_owned()),
            ty::ConstKind::Value(rustc_const_eval::interpret::ConstValue::ByRef { .. }) => Err("encountered ref constant value".to_owned()),

            ty::ConstKind::Param(param) => Ok(ast::mk::const_ident(sp, Ident::new(param.name, sp))),
            ty::ConstKind::Bound(_, _) => Err("encountered constant bound variable".to_owned()),
            ty::ConstKind::Infer(_) => Err("encountered constant inference variable".to_owned()),

            ty::ConstKind::Unevaluated(_) => Err("encountered unevaluated constant".to_owned()),
            ty::ConstKind::Placeholder(_) => Err("encountered placeholder constant".to_owned()),
            ty::ConstKind::Error(_) => Err("encountered constant error".to_owned()),
        }
    }

    fn print_region(self, region: ty::Region<'_>) -> Result<Self::Region, Self::Error> {
        let sp = self.sp;

        match *region {
            ReStatic => Ok(Some(ast::mk::lifetime(sp, Ident::new(kw::StaticLifetime, sp)))),

            ReEarlyBound(region) => {
                if region.name == kw::Empty { return Ok(None); }
                Ok(Some(ast::mk::lifetime(sp, Ident::new(region.name, sp))))
            }

            | ReLateBound(_, ty::BoundRegion { kind: region, .. })
            | ReFree(ty::FreeRegion { bound_region: region, .. })
            | RePlaceholder(ty::Placeholder { name: region, .. }) => {
                let ty::BoundRegionKind::BrNamed(_, region_name) = region else { return Ok(None); };
                if region_name == kw::Empty || region_name == kw::UnderscoreLifetime { return Ok(None); }
                Ok(Some(ast::mk::lifetime(sp, Ident::new(region_name, sp))))
            }

            ReVar(_) | ReEmpty(_) | ReErased => Ok(None),
        }
    }

    fn print_def_path(
        self,
        def_id: hir::DefId,
        substs: &'tcx [ty::GenericArg<'tcx>],
    ) -> Result<Self::Path, Self::Error> {
        if let DefPathHandling::PreferVisible(scoped_item_paths) | DefPathHandling::ForceVisible(scoped_item_paths) = self.def_path_handling {
            if let Some(path) = self.try_print_visible_def_path(def_id, scoped_item_paths)? {
                if substs.is_empty() { return Ok(path); }
                return self.path_generic_args(|_| Ok(path), substs);
            }

            if let DefPathHandling::ForceVisible(_) = self.def_path_handling {
                return Err("cannot find visible path for definition".to_owned());
            }
        }

        self.default_print_def_path(def_id, substs)
    }

    fn path_crate(self, cnum: hir::CrateNum) -> Result<Self::Path, Self::Error> {
        match cnum {
            LOCAL_CRATE => Ok(ast::mk::path_ident(self.sp, Ident::new(kw::Crate, self.sp))),
            _ => {
                let crate_name = self.tcx.crate_name(cnum);
                Ok(ast::mk::path_ident(self.sp, Ident::new(crate_name, self.sp)))
            }
        }
    }

    fn path_qualified(self, self_ty: Ty<'tcx>, trait_ref: Option<ty::TraitRef<'tcx>>) -> Result<Self::Path, Self::Error> {
        let self_ty_ast = self.print_type(self_ty)?.into_inner();
        let ast::TyKind::Path(None, self_ty_path) = self_ty_ast.kind else { return Err("encountered non-path type".to_owned()) };

        if let None = trait_ref
            && let ty::TyKind::Adt(..) | ty::TyKind::Foreign(..) | ty::TyKind::Bool | ty::TyKind::Char | ty::TyKind::Int(..) | ty::TyKind::Uint(..) | ty::TyKind::Float(..) = self_ty.kind()
        {
            return Ok(self_ty_path);
        }

        // TODO: Switch to using `Self::Path = (Option<ast::QSelf>, ast::Path)` to encode qualified paths.
        Err("encountered qualified path".to_owned())
    }

    fn path_append_impl(
        self,
        _print_prefix: impl FnOnce(Self) -> Result<Self::Path, Self::Error>,
        _disambiguated_data: &hir::definitions::DisambiguatedDefPathData,
        _self_ty: Ty<'tcx>,
        _trait_ref: Option<ty::TraitRef<'tcx>>,
    ) -> Result<Self::Path, Self::Error> {
        Err("encountered impl".to_owned())
    }

    fn path_append(
        self,
        print_prefix: impl FnOnce(Self) -> Result<Self::Path, Self::Error>,
        disambiguated_data: &hir::definitions::DisambiguatedDefPathData,
    ) -> Result<Self::Path, Self::Error> {
        let mut path = print_prefix(self)?;

        let hir::definitions::DefPathDataName::Named(name) = disambiguated_data.data.name() else {
            return Err("encountered anonymous, ambiguous path segment".to_owned());
        };

        path = ast::mk::pathx(self.sp, path, vec![Ident::new(name, self.sp)]);

        Ok(path)
    }

    fn path_generic_args(
        self,
        print_prefix: impl FnOnce(Self) -> Result<Self::Path, Self::Error>,
        args: &[ty::subst::GenericArg<'tcx>],
    ) -> Result<Self::Path, Self::Error> {
        let mut path = print_prefix(self)?;

        let args_ast = args.iter()
            .map(|arg| -> Result<_, Self::Error> {
                match arg.unpack() {
                    ty::subst::GenericArgKind::Type(ty) => {
                        let ty_ast = self.print_type(ty)?;
                        Ok(Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Type(ty_ast))))
                    }
                    ty::subst::GenericArgKind::Const(ct) => {
                        let const_ast = self.print_const(ct)?;
                        Ok(Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Const(const_ast))))
                    }
                    ty::subst::GenericArgKind::Lifetime(region) => {
                        let Some(lifetime) = self.print_region(region)? else { return Ok(None); };
                        Ok(Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Lifetime(lifetime))))
                    }
                }
            })
            .flat_map(Result::transpose)
            .try_collect()?;

        let Some(last_segment) = path.segments.last_mut() else { return Err("encountered empty path".to_owned()) };
        last_segment.args = Some(P(ast::GenericArgs::AngleBracketed(ast::AngleBracketedArgs { span: self.sp, args: args_ast })));

        Ok(path)
    }
}

trait PrinterExt<'tcx>: ty::print::Printer<'tcx> + Copy {
    fn path_local_root(self) -> Result<Self::Path, Self::Error>;

    fn try_print_visible_def_path(self, def_id: hir::DefId, scoped_item_paths: ScopedItemPaths) -> Result<Option<Self::Path>, Self::Error> {
        fn try_print_visible_def_path_impl<'tcx, T: PrinterExt<'tcx>>(
            printer: T,
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

            if def_id.is_local() {
                if let Some(parent) = printer.tcx().opt_parent(def_id) {
                    match printer.tcx().def_kind(parent) {
                        | hir::def::DefKind::Const
                        | hir::def::DefKind::Static(..)
                        | hir::def::DefKind::Fn
                        | hir::def::DefKind::AssocFn => {
                            match scoped_item_paths {
                                ScopedItemPaths::Trimmed => {
                                    let disambiguated_data = printer.tcx().def_key(def_id).disambiguated_data;
                                    let path = printer.path_append(|_| printer.path_local_root(), &disambiguated_data)?;
                                    return Ok(Some(path));
                                }
                                ScopedItemPaths::FullyQualified => {}
                            }
                        }
                        _ => {}
                    }
                }

                return Ok(None);
            }

            let visible_parent_map = printer.tcx().visible_parent_map(());
            let mut cur_def_key = printer.tcx().def_key(def_id);

            // Constructors are unnamed by themselves. We must use the name of their parent instead.
            if let hir::definitions::DefPathData::Ctor = cur_def_key.disambiguated_data.data {
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
                hir::definitions::DefPathData::TypeNs(ref mut name) if Some(visible_parent) != actual_parent => {
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
                hir::definitions::DefPathData::CrateRoot => {
                    data = hir::definitions::DefPathData::TypeNs(printer.tcx().crate_name(def_id.krate));
                }
                _ => {}
            }

            if callers.contains(&visible_parent) { return Ok(None); }
            callers.push(visible_parent);
            let Some(path) = try_print_visible_def_path_impl(printer, visible_parent, scoped_item_paths, callers)? else { return Ok(None); };
            callers.pop();

            let disambiguated_data = hir::definitions::DisambiguatedDefPathData { data, disambiguator: 0 };
            let path = printer.path_append(|_| Ok(path), &disambiguated_data)?;
            Ok(Some(path))
        }

        let mut callers = vec![];
        try_print_visible_def_path_impl(self, def_id, scoped_item_paths, &mut callers)
    }
}

impl<'tcx> PrinterExt<'tcx> for AstTyPrinter<'tcx> {
    fn path_local_root(self) -> Result<Self::Path, Self::Error> {
        Ok(ast::Path { span: self.sp, segments: vec![], tokens: None })
    }
}

pub fn ast_repr<'tcx>(tcx: TyCtxt<'tcx>, sp: Span, ty: Ty<'tcx>, def_path_handling: DefPathHandling, opaque_ty_handling: OpaqueTyHandling) -> Option<P<ast::Ty>> {
    let printer = AstTyPrinter {
        tcx,
        sp,
        def_path_handling,
        opaque_ty_handling,
    };
    printer.print_type(ty).ok()
}
