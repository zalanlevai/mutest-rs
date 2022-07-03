pub use rustc_middle::ty::*;

use rustc_middle::ty;
use rustc_middle::ty::print::Printer;
use rustc_infer::infer::TyCtxtInferExt;

use crate::analysis::hir::{self, LOCAL_CRATE};
use crate::codegen::ast::{self, P};
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, sym, kw};

pub fn impls_trait_params<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId) -> bool {
    tcx.infer_ctxt().enter(|infcx| {
        rustc_trait_selection::traits::type_known_to_meet_bound_modulo_regions(&infcx, param_env, ty, trait_def_id, DUMMY_SP)
    })
}

pub fn impls_trait<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId) -> bool {
    impls_trait_params(tcx, ty::ParamEnv::empty(), ty, trait_def_id)
}

#[derive(Clone, Copy)]
struct AstTyPrinter<'tcx> {
    tcx: TyCtxt<'tcx>,
    sp: Span,
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

            ty::TyKind::Param(param) => Ok(ast::mk::ty_ident(sp, None, Ident::new(param.name, sp))),
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
                let def_path = self.print_def_path(def_id, substs)?;
                Ok(ast::mk::ty(sp, ast::TyKind::ImplTrait(ast::DUMMY_NODE_ID, vec![
                    ast::mk::trait_bound(ast::TraitBoundModifier::None, def_path)
                ])))
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
                    ext: ast::Extern::None,
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
                if let Ok(v) = scalar.to_bool() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Bool(v)).into_inner().kind));
                }
                if let Ok(v) = scalar.to_char() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Char(v)).into_inner().kind));
                }
                if let Ok(v) = scalar.to_i8() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I8))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_i16() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I16))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_i32() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I32))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_i64() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I64))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_i128() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Signed(ast::IntTy::I128))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_u8() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U8))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_u16() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U16))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_u32() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U32))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_u64() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U64))).into_inner().kind));
                }
                if let Ok(v) = scalar.to_u128() {
                    return Ok(ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::LitKind::Int(v as u128, ast::LitIntType::Unsigned(ast::UintTy::U128))).into_inner().kind));
                }

                Err("encountered unknown constant scalar value".to_owned())
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

pub fn ast_repr<'tcx>(tcx: TyCtxt<'tcx>, sp: Span, ty: Ty<'tcx>) -> Option<P<ast::Ty>> {
    let printer = AstTyPrinter {
        tcx,
        sp,
    };
    printer.print_type(ty).ok()
}
