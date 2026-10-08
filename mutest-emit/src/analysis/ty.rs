pub use rustc_middle::ty::*;

use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::ty;
use rustc_trait_selection::infer::InferCtxtExt;

use crate::analysis::hir;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol};

pub fn impls_trait<'tcx>(tcx: TyCtxt<'tcx>, body_def_id: hir::LocalDefId, ty: Ty<'tcx>, trait_def_id: hir::DefId, args: Vec<ty::GenericArg<'tcx>>) -> bool {
    let ty = tcx.erase_and_anonymize_regions(ty);
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
            let proj = Ty::new_projection(tcx, ty::IsRigid::No, assoc_item.def_id, args);
            tcx.try_normalize_erasing_regions(typing_env, ty::Unnormalized::new(proj)).ok()
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

pub mod print {
    use std::iter;

    use rustc_data_structures::thin_vec::{ThinVec, thin_vec};
    use rustc_infer::infer::TyCtxtInferExt;
    use rustc_middle::mir;
    use rustc_middle::ty::{self, Ty, TyCtxt};
    use rustc_span::{DUMMY_SP, Ident, Span, Symbol, sym, kw};

    use crate::analysis::ast_lowering;
    use crate::analysis::hir;
    use crate::analysis::res;
    use crate::codegen::ast;
    use crate::codegen::hygiene;

    use super::SpanFromGenericsExt;

    fn mk_ty_ast(sp: Span, kind: ast::TyKind) -> Box<ast::Ty> {
        Box::new(ast::Ty { id: ast::DUMMY_NODE_ID, span: sp, kind })
    }

    pub fn mk_ty_path_ast(q_self: Option<Box<ast::QSelf>>, path: ast::Path) -> Box<ast::Ty> {
        mk_ty_ast(path.span, ast::TyKind::Path(q_self, path))
    }

    pub fn mk_ty_ident_ast(sp: Span, q_self: Option<Box<ast::QSelf>>, ident: Ident) -> Box<ast::Ty> {
        mk_ty_path_ast(q_self, ast::Path {
            span: sp,
            segments: thin_vec![
                ast::PathSegment {
                    id: ast::DUMMY_NODE_ID,
                    ident: ident.with_span_pos(sp),
                    args: None,
                },
            ],
        })
    }

    fn mk_expr_kind_int_ast(sp: Span, i: isize, suffix: Symbol) -> ast::ExprKind {
        let abs_symbol = Symbol::intern(&i.abs().to_string());
        let abs_lit_expr_kind = ast::ExprKind::Lit(ast::token::Lit::new(ast::token::LitKind::Integer, abs_symbol, Some(suffix)));

        match i {
            0.. => abs_lit_expr_kind,
            _ => ast::ExprKind::Unary(ast::UnOp::Neg, Box::new(ast::Expr {
                id: ast::DUMMY_NODE_ID,
                span: sp,
                attrs: ast::AttrVec::new(),
                kind: abs_lit_expr_kind,
                tokens: None,
            })),
        }
    }

    fn mk_expr_kind_float_ast(sp: Span, v: f64, suffix: Symbol) -> ast::ExprKind {
        let abs_symbol = Symbol::intern(&v.abs().to_string());
        let abs_lit_expr_kind = ast::ExprKind::Lit(ast::token::Lit::new(ast::token::LitKind::Float, abs_symbol, Some(suffix)));

        match v {
            0_f64.. => abs_lit_expr_kind,
            _ => ast::ExprKind::Unary(ast::UnOp::Neg, Box::new(ast::Expr {
                id: ast::DUMMY_NODE_ID,
                span: sp,
                attrs: ast::AttrVec::new(),
                kind: abs_lit_expr_kind,
                tokens: None,
            })),
        }
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
        opaque_ty_handling: OpaqueTyHandling,
        binding_item_def_id: hir::DefId,
    }

    impl<'tcx, 'op> AstTyPrinter<'tcx, 'op> {
        fn path_generic_args(
            &mut self,
            mut path: ast::Path,
            args: &[ty::GenericArg<'tcx>],
            assoc_constraints: impl Iterator<Item = ty::ExistentialProjection<'tcx>>,
        ) -> Result<ast::Path, String> {
            let sp = self.sp;

            let mut errors = vec![];

            let mut args_ast = args.iter()
                .map(|arg: &ty::GenericArg<'tcx>| -> Result<Option<ast::AngleBracketedArg>, String> {
                    match arg.kind() {
                        ty::GenericArgKind::Type(ty) => {
                            let ty_ast = match self.print_ty(ty) {
                                Ok(ty_ast) => ty_ast,
                                Err(error) => {
                                    let mut diagnostic = self.tcx.dcx().struct_span_warn(sp, format!("replacing unrepresentable generic type argument `{ty}` with `_`"));
                                    diagnostic.span_label(sp, error);
                                    diagnostic.emit();

                                    mk_ty_ast(sp, ast::TyKind::Infer)
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
                .map(|assoc_constraint| -> Result<_, String> {
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
            last_segment.args = Some(Box::new(ast::GenericArgs::AngleBracketed(ast::AngleBracketedArgs { span: self.sp, args: args_ast })));

            Ok(path)
        }

        fn print_def_path(&mut self, def_id: hir::DefId, args: &'tcx [ty::GenericArg<'tcx>]) -> Result<ast::Path, String> {
            let Ok(def_path) = res::visible_def_path(self.tcx, self.crate_res, res::DefPathRequestKind::Def(def_id), self.scope, None, self.sp) else {
                return Err(format!("encountered definition `{}` with no visible path", self.tcx.def_path_str(def_id)));
            };
            let (None, mut path) = def_path.unhygienic_ast_path(self.crate_res, self) else {
                return Err("encountered definition with qualified path in type position".to_owned());
            };
            // HACK: This is inefficient, as it resolves the path again, which already happens in `visible_def_path`.
            hygiene::sanitize_path(self.tcx, self.crate_res, self.def_res, self.scope, &mut path, hir::Res::Def(self.tcx.def_kind(def_id), def_id), false);

            if args.is_empty() { return Ok(path); }
            let item_args = self.tcx.generics_of(def_id).own_args(args);
            if !item_args.is_empty() {
                path = self.path_generic_args(path, item_args, iter::empty())?;
            }

            // NOTE: If the def is a trait assoc item, then we must also write out the trait generics on the trait path segment.
            if let Some(assoc_item) = self.tcx.opt_associated_item(def_id) && let Some(trait_def_id) = assoc_item.trait_container(self.tcx) {
                // HACK: Temporarily turn the path into a path to the containing trait itself to append generic args to it.
                let Some(assoc_item_path_segment) = path.segments.pop() else { unreachable!("empty path") };

                let trait_args = self.tcx.generics_of(trait_def_id).own_args(args);
                if !trait_args.is_empty() {
                    path = self.path_generic_args(path, trait_args, iter::empty())?;
                }

                path.segments.push(assoc_item_path_segment);
            }

            Ok(path)
        }

        fn print_region(&mut self, region: ty::Region<'tcx>) -> Result<Option<ast::Lifetime>, String> {
            let sp = self.sp;

            match region.kind() {
                ty::RegionKind::ReStatic => Ok(Some(ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: Ident::new(kw::StaticLifetime, sp) })),

                ty::RegionKind::ReEarlyParam(early_param_region) => {
                    if early_param_region.name == sym::empty { return Ok(None); }

                    let mut ident = Ident::new(early_param_region.name, sp);

                    let generic_param_def = self.tcx.generics_of(self.binding_item_def_id).region_param(early_param_region, self.tcx);
                    if generic_param_def.is_anonymous_lifetime() {
                        hygiene::generate_revealed_name_for_anonymous_region(&mut ident, generic_param_def);
                    } else {
                        let def_ident_span = self.tcx.def_ident_span(generic_param_def.def_id).unwrap_or(DUMMY_SP);
                        hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                    }

                    Ok(Some(ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: ident.with_span_pos(sp) }))
                }

                | ty::RegionKind::ReBound(_, ty::BoundRegion { kind: bound_region_kind, .. })
                | ty::RegionKind::RePlaceholder(ty::Placeholder { bound: ty::BoundRegion { kind: bound_region_kind, .. }, .. })
                => {
                    let ty::BoundRegionKind::Named(def_id) = bound_region_kind else { return Ok(None); };
                    let region_name = self.tcx.item_name(def_id);
                    if region_name == sym::empty || region_name == kw::UnderscoreLifetime { return Ok(None); }

                    let mut ident = Ident::new(region_name, sp);

                    let def_ident_span = self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                    hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);

                    Ok(Some(ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: ident.with_span_pos(sp) }))
                }

                ty::RegionKind::ReLateParam(ty::LateParamRegion { kind: late_param_region_kind, .. }) => {
                    let ty::LateParamRegionKind::Named(def_id) = late_param_region_kind else { return Ok(None); };
                    let region_name = self.tcx.item_name(def_id);
                    if region_name == sym::empty || region_name == kw::UnderscoreLifetime { return Ok(None); }

                    let mut ident = Ident::new(region_name, sp);

                    let def_ident_span = self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                    hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);

                    Ok(Some(ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: ident.with_span_pos(sp) }))
                }

                ty::RegionKind::ReVar(_) => Ok(None),

                ty::RegionKind::ReErased => Ok(Some(ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: Ident::new(kw::UnderscoreLifetime, sp) })),

                ty::RegionKind::ReError(_) => Err("encountered region error".to_owned()),
            }
        }

        fn print_const(&mut self, ct: ty::Const<'tcx>) -> Result<ast::AnonConst, String> {
            fn eval_const<'tcx>(tcx: TyCtxt<'tcx>, ct: ty::Const<'tcx>, sp: Span) -> Result<ast::AnonConst, String> {
                let infcx = tcx.infer_ctxt().build(ty::TypingMode::PostAnalysis);
                let value = rustc_trait_selection::traits::evaluate_const(&infcx, ct, ty::ParamEnv::empty()).try_to_value().ok_or_else(|| "encountered invalid const".to_owned())?;
                let val = tcx.valtree_to_const_val(value);

                match val {
                    mir::ConstValue::Scalar(scalar) => {
                        let lit_expr_kind = match value.ty.kind() {
                            ty::TyKind::Bool => {
                                scalar.to_bool()
                                    .map(|v| {
                                        let symbol = match v {
                                            true => kw::True,
                                            false => kw::False,
                                        };
                                        ast::ExprKind::Lit(ast::token::Lit::new(ast::token::LitKind::Bool, symbol, None))
                                    })
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid bool const: {e:?}"))
                            }
                            ty::TyKind::Char => {
                                scalar.to_char()
                                    .map(|v| {
                                        let symbol = Symbol::intern(&v.to_string());
                                        ast::ExprKind::Lit(ast::token::Lit::new(ast::token::LitKind::Char, symbol, None))
                                    })
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid char const: {e:?}"))
                            }
                            ty::TyKind::Int(ty::IntTy::I8) => {
                                scalar.to_i8().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::i8))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid i8 const: {e:?}"))
                            }
                            ty::TyKind::Int(ty::IntTy::I16) => {
                                scalar.to_i16().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::i16))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid i16 const: {e:?}"))
                            }
                            ty::TyKind::Int(ty::IntTy::I32) => {
                                scalar.to_i32().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::i32))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid i32 const: {e:?}"))
                            }
                            ty::TyKind::Int(ty::IntTy::I64) => {
                                scalar.to_i64().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::i64))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid i64 const: {e:?}"))
                            }
                            ty::TyKind::Int(ty::IntTy::I128) => {
                                scalar.to_i128().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::i128))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid i128 const: {e:?}"))
                            }
                            ty::TyKind::Int(ty::IntTy::Isize) => {
                                scalar.to_target_isize(&tcx).map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::isize))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid isize const: {e:?}"))
                            }
                            ty::TyKind::Uint(ty::UintTy::U8) => {
                                scalar.to_u8().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::u8))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid u8 const: {e:?}"))
                            }
                            ty::TyKind::Uint(ty::UintTy::U16) => {
                                scalar.to_u16().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::u16))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid u16 const: {e:?}"))
                            }
                            ty::TyKind::Uint(ty::UintTy::U32) => {
                                scalar.to_u32().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::u32))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid u32 const: {e:?}"))
                            }
                            ty::TyKind::Uint(ty::UintTy::U64) => {
                                scalar.to_u64().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::u64))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid u64 const: {e:?}"))
                            }
                            ty::TyKind::Uint(ty::UintTy::U128) => {
                                scalar.to_u128().map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::u128))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid u128 const: {e:?}"))
                            }
                            ty::TyKind::Uint(ty::UintTy::Usize) => {
                                scalar.to_target_usize(&tcx).map(|v| mk_expr_kind_int_ast(sp, v as isize, sym::usize))
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid usize const: {e:?}"))
                            }
                            ty::TyKind::Float(ty::FloatTy::F16) => {
                                scalar.to_f16()
                                    .map(|v| {
                                        let v = f16::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u16);
                                        mk_expr_kind_float_ast(sp, v as f64, sym::f16)
                                    })
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid f16 const: {e:?}"))
                            }
                            ty::TyKind::Float(ty::FloatTy::F32) => {
                                scalar.to_f32()
                                    .map(|v| {
                                        let v = f32::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u32);
                                        mk_expr_kind_float_ast(sp, v as f64, sym::f32)
                                    })
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid f32 const: {e:?}"))
                            }
                            ty::TyKind::Float(ty::FloatTy::F64) => {
                                scalar.to_f64()
                                    .map(|v| {
                                        let v = f64::from_bits(rustc_apfloat::ieee::Semantics::to_bits(v) as u64);
                                        mk_expr_kind_float_ast(sp, v, sym::f64)
                                    })
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid f64 const: {e:?}"))
                            }
                            ty::TyKind::Float(ty::FloatTy::F128) => {
                                scalar.to_f128()
                                    .map(|v| {
                                        let rounded_v: rustc_apfloat::ieee::Double = rustc_apfloat::FloatConvert::convert(v, &mut false).value;
                                        let v = f64::from_bits(rustc_apfloat::ieee::Semantics::to_bits(rounded_v) as u64);
                                        mk_expr_kind_float_ast(sp, v, sym::f128)
                                    })
                                    .report_err()
                                    .map_err(|e| format!("encountered invalid f128 const: {e:?}"))
                            }
                            _ => Err("encountered unknown constant scalar value".to_owned())
                        }?;

                        Ok(ast::AnonConst {
                            id: ast::DUMMY_NODE_ID,
                            value: Box::new(ast::Expr {
                                id: ast::DUMMY_NODE_ID,
                                span: sp,
                                attrs: ast::AttrVec::new(),
                                kind: lit_expr_kind,
                                tokens: None,
                            }),
                        })
                    }

                    mir::ConstValue::ZeroSized => Err("encountered zero-sized const".to_owned()),
                    mir::ConstValue::Slice { .. } => Err("encountered slice const".to_owned()),
                    mir::ConstValue::Indirect { .. } => Err("encountered indirect const".to_owned()),
                }
            }

            let sp = self.sp;

            match ct.kind() {
                ty::ConstKind::Param(param_const) => {
                    let mut ident = Ident::new(param_const.name, sp);
                    'sanitize: {
                        let Some(scope) = self.scope else { break 'sanitize; };
                        let def_ident_span = param_const.span_from_generics(self.tcx, scope);
                        hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                    }

                    Ok(ast::AnonConst {
                        id: ast::DUMMY_NODE_ID,
                        value: Box::new(ast::Expr {
                            id: ast::DUMMY_NODE_ID,
                            span: sp,
                            attrs: ast::AttrVec::new(),
                            kind: ast::ExprKind::Path(None, ast::Path {
                                span: sp,
                                segments: thin_vec![
                                    ast::PathSegment {
                                        id: ast::DUMMY_NODE_ID,
                                        ident: ident.with_span_pos(sp),
                                        args: None,
                                    },
                                ],
                            }),
                            tokens: None,
                        }),
                    })
                }

                ty::ConstKind::Alias(_, alias_const) => {
                    match alias_const.kind {
                        ty::AliasConstKind::Projection { def_id } => {
                            let def_path = self.print_def_path(def_id, alias_const.args)?;

                            // HACK: `self_ty` is not available on AliasConst, so we get it manually.
                            let self_ty = self.print_ty(alias_const.args.type_at(0))?;
                            let qself = Box::new(ast::QSelf {
                                ty: self_ty,
                                path_span: DUMMY_SP,
                                position: def_path.segments.len() - 1,
                            });

                            Ok(ast::AnonConst {
                                id: ast::DUMMY_NODE_ID,
                                value: Box::new(ast::Expr {
                                    id: ast::DUMMY_NODE_ID,
                                    span: sp,
                                    attrs: ast::AttrVec::new(),
                                    kind: ast::ExprKind::Path(Some(qself), def_path),
                                    tokens: None,
                                }),
                            })
                        }
                        ty::AliasConstKind::Inherent { def_id } | ty::AliasConstKind::Free { def_id } => {
                            let def_path = self.print_def_path(def_id, alias_const.args)?;
                            Ok(ast::AnonConst {
                                id: ast::DUMMY_NODE_ID,
                                value: Box::new(ast::Expr {
                                    id: ast::DUMMY_NODE_ID,
                                    span: sp,
                                    attrs: ast::AttrVec::new(),
                                    kind: ast::ExprKind::Path(None, def_path),
                                    tokens: None,
                                }),
                            })
                        }
                        ty::AliasConstKind::Anon { def_id: _ } => {
                            eval_const(self.tcx, ct, self.sp)
                        }
                    }
                }

                | ty::ConstKind::Infer(_)
                | ty::ConstKind::Bound(_, _)
                | ty::ConstKind::Placeholder(_)
                | ty::ConstKind::Value(_)
                | ty::ConstKind::Expr(_)
                => {
                    eval_const(self.tcx, ct, self.sp)
                }

                ty::ConstKind::Error(_) => Err("encountered const error".to_owned()),
            }
        }

        fn print_dyn_existential(&mut self, predicates: &'tcx ty::List<ty::PolyExistentialPredicate<'tcx>>) -> Result<Box<ast::Ty>, String> {
            let sp = self.sp;

            let principal = predicates.principal().map_or(Ok(None), |principal| -> Result<_, String> {
                let principal = principal.skip_binder();

                let mut def_path = self.print_def_path(principal.def_id, &[])?;

                // Fn(...) -> ...
                if let Some(_) = self.tcx.fn_trait_kind_from_def_id(principal.def_id)
                    && let ty::Tuple(input_tys) = principal.args.type_at(0).kind()
                    && let mut projections = predicates.projection_bounds()
                    && let (Some(projection), None) = (projections.next(), projections.next())
                {
                    let output_ty = projection.skip_binder().term.as_type();

                    let input_tys_ast = input_tys.iter().map(|ty| self.print_ty(ty)).try_collect()?;
                    let output_ty_ast = output_ty.map_or(Result::<_, String>::Ok(None), |ty| Ok(Some(self.print_ty(ty)?)))?;
                    let args = Box::new(ast::GenericArgs::Parenthesized(ast::ParenthesizedArgs {
                        span: sp,
                        inputs: input_tys_ast,
                        inputs_span: sp,
                        output: match output_ty_ast {
                            Some(ty) => ast::FnRetTy::Ty(ty),
                            None => ast::FnRetTy::Default(sp),
                        },
                    }));
                    def_path.segments.last_mut().unwrap().args = Some(args);
                    return Ok(Some(ast::GenericBound::Trait(ast::PolyTraitRef {
                        span: sp,
                        parens: ast::Parens::No,
                        bound_generic_params: ThinVec::new(),
                        modifiers: ast::TraitBoundModifiers::NONE,
                        trait_ref: ast::TraitRef { ref_id: ast::DUMMY_NODE_ID, path: def_path },
                    })));
                }

                let dummy_self_ty = Ty::new_fresh(self.tcx, 0);
                let principal = principal.with_self_ty(self.tcx, dummy_self_ty);

                let args = self.tcx.generics_of(principal.def_id).own_args_no_defaults(self.tcx, principal.args);
                let assoc_constraints = predicates.projection_bounds().map(|bounds| bounds.skip_binder());
                let path = self.path_generic_args(def_path, args, assoc_constraints)?;
                Ok(Some(ast::GenericBound::Trait(ast::PolyTraitRef {
                    span: sp,
                    parens: ast::Parens::No,
                    bound_generic_params: ThinVec::new(),
                    modifiers: ast::TraitBoundModifiers::NONE,
                    trait_ref: ast::TraitRef { ref_id: ast::DUMMY_NODE_ID, path },
                })))
            })?;

            let auto_traits = predicates.auto_traits()
                .map(|def_id| -> Result<_, String> {
                    let def_path = self.print_def_path(def_id, &[])?;
                    Ok(ast::GenericBound::Trait(ast::PolyTraitRef {
                        span: sp,
                        parens: ast::Parens::No,
                        bound_generic_params: ThinVec::new(),
                        modifiers: ast::TraitBoundModifiers::NONE,
                        trait_ref: ast::TraitRef { ref_id: ast::DUMMY_NODE_ID, path: def_path },
                    }))
                })
                .try_collect::<Vec<_>>()?;

            let bounds = principal.into_iter()
                .chain(auto_traits.into_iter())
                .collect::<ThinVec<_>>();

            Ok(mk_ty_ast(sp, ast::TyKind::TraitObject(bounds, ast::TraitObjectSyntax::Dyn)))
        }

        pub fn print_ty(&mut self, ty: Ty<'tcx>) -> Result<Box<ast::Ty>, String> {
            let sp = self.sp;

            match *ty.kind() {
                ty::TyKind::Bool => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::bool, sp))),
                ty::TyKind::Char => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::char, sp))),
                ty::TyKind::Int(ty::IntTy::I8) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::i8, sp))),
                ty::TyKind::Int(ty::IntTy::I16) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::i16, sp))),
                ty::TyKind::Int(ty::IntTy::I32) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::i32, sp))),
                ty::TyKind::Int(ty::IntTy::I64) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::i64, sp))),
                ty::TyKind::Int(ty::IntTy::I128) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::i128, sp))),
                ty::TyKind::Int(ty::IntTy::Isize) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::isize, sp))),
                ty::TyKind::Uint(ty::UintTy::U8) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::u8, sp))),
                ty::TyKind::Uint(ty::UintTy::U16) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::u16, sp))),
                ty::TyKind::Uint(ty::UintTy::U32) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::u32, sp))),
                ty::TyKind::Uint(ty::UintTy::U64) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::u64, sp))),
                ty::TyKind::Uint(ty::UintTy::U128) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::u128, sp))),
                ty::TyKind::Uint(ty::UintTy::Usize) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::usize, sp))),
                ty::TyKind::Float(ty::FloatTy::F16) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::f16, sp))),
                ty::TyKind::Float(ty::FloatTy::F32) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::f32, sp))),
                ty::TyKind::Float(ty::FloatTy::F64) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::f64, sp))),
                ty::TyKind::Float(ty::FloatTy::F128) => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::f128, sp))),
                ty::TyKind::Str => Ok(mk_ty_ident_ast(sp, None, Ident::new(sym::str, sp))),
                ty::TyKind::Never => Ok(mk_ty_ast(sp, ast::TyKind::Never)),

                ty::TyKind::RawPtr(ty, mutbl) => {
                    let inner_ty = self.print_ty(ty)?;
                    Ok(mk_ty_ast(sp, ast::TyKind::Ptr(ast::MutTy { ty: inner_ty, mutbl })))
                }
                ty::TyKind::Ref(region, ty, mutbl) => {
                    let inner_ty = self.print_ty(ty)?;
                    let lifetime = self.print_region(region)?;
                    Ok(mk_ty_ast(sp, ast::TyKind::Ref(lifetime, ast::MutTy { ty: inner_ty, mutbl })))
                }
                ty::TyKind::Tuple(tys) => {
                    let inner_tys = tys.iter().map(|ty| self.print_ty(ty)).try_collect()?;
                    Ok(mk_ty_ast(sp, ast::TyKind::Tup(inner_tys)))
                }
                ty::TyKind::Slice(ty) => {
                    let inner_ty = self.print_ty(ty)?;
                    Ok(mk_ty_ast(sp, ast::TyKind::Slice(inner_ty)))
                }
                ty::TyKind::Array(ty, size) => {
                    let inner_ty = self.print_ty(ty)?;
                    let size_const = self.print_const(size)?;
                    Ok(mk_ty_ast(sp, ast::TyKind::Array(inner_ty, size_const)))
                }

                ty::TyKind::Adt(def, args) => {
                    let def_path = self.print_def_path(def.did(), args)?;
                    Ok(mk_ty_path_ast(None, def_path))
                }
                ty::TyKind::Foreign(def_id) => {
                    // TODO
                    let def_path = self.print_def_path(def_id, &[])?;
                    Ok(mk_ty_path_ast(None, def_path))
                }
                ty::TyKind::Dynamic(predicates, region) => {
                    let mut dyn_existential = self.print_dyn_existential(predicates)?;
                    let ast::TyKind::TraitObject(bounds, _syntax) = &mut dyn_existential.kind else { unreachable!() };
                    if let Some(lifetime) = self.print_region(region)? {
                        bounds.push(ast::GenericBound::Outlives(lifetime));
                    }
                    // NOTE: `dyn` trait objects of multiple bounds are syntactically ambiguous in some positions
                    //       unless surrounded by parens.
                    if bounds.len() > 1 {
                        dyn_existential = mk_ty_ast(sp, ast::TyKind::Paren(dyn_existential));
                    }
                    Ok(dyn_existential)
                }
                ty::TyKind::Alias(_, alias_ty) => {
                    match alias_ty.kind {
                        ty::AliasTyKind::Opaque { def_id } => {
                            match self.opaque_ty_handling {
                                OpaqueTyHandling::Infer => Ok(mk_ty_ast(sp, ast::TyKind::Infer)),
                                OpaqueTyHandling::Keep => {
                                    let def_path = self.print_def_path(def_id, alias_ty.args)?;
                                    Ok(mk_ty_ast(sp, ast::TyKind::ImplTrait(ast::DUMMY_NODE_ID, thin_vec![
                                        ast::GenericBound::Trait(ast::PolyTraitRef {
                                            span: def_path.span,
                                            parens: ast::Parens::No,
                                            bound_generic_params: ThinVec::new(),
                                            modifiers: ast::TraitBoundModifiers::NONE,
                                            trait_ref: ast::TraitRef { ref_id: ast::DUMMY_NODE_ID, path: def_path },
                                        }),
                                    ])))
                                }
                                OpaqueTyHandling::Resolve => {
                                    let ty = self.tcx.type_of(def_id).instantiate_identity().skip_normalization();
                                    self.print_ty(ty)
                                }
                            }
                        }
                        ty::AliasTyKind::Projection { def_id } => {
                            let def_path = self.print_def_path(def_id, alias_ty.args)?;

                            let self_ty = self.print_ty(alias_ty.self_ty())?;
                            let qself = Box::new(ast::QSelf {
                                ty: self_ty,
                                path_span: DUMMY_SP,
                                position: def_path.segments.len() - 1,
                            });

                            Ok(mk_ty_path_ast(Some(qself), def_path))
                        }
                        ty::AliasTyKind::Inherent { def_id } | ty::AliasTyKind::Free { def_id } => {
                            let def_path = self.print_def_path(def_id, alias_ty.args)?;
                            Ok(mk_ty_path_ast(None, def_path))
                        }
                    }
                }
                ty::TyKind::Param(param_ty) => {
                    // Avoid naming synthetic generic params from `impl Trait` function parameters.
                    if param_ty.name.as_str().starts_with("impl ") {
                        return Ok(mk_ty_ast(sp, ast::TyKind::Infer));
                    }

                    let mut ident = Ident::new(param_ty.name, sp);
                    'sanitize: {
                        let Some(scope) = self.scope else { break 'sanitize; };
                        if ident.name == kw::SelfUpper { break 'sanitize; }
                        let def_ident_span = param_ty.span_from_generics(self.tcx, scope);
                        hygiene::sanitize_ident_if_def_from_expansion(&mut ident, def_ident_span);
                    }
                    Ok(mk_ty_ident_ast(sp, None, ident))
                }

                ty::TyKind::FnPtr(fn_sig_tys, fn_header) => {
                    let fn_sig_tys = fn_sig_tys.skip_binder();

                    let input_tys_ast = fn_sig_tys.inputs().iter().copied().map(|ty| self.print_ty(ty)).try_collect::<Vec<_>>()?;
                    let output_ty_ast = self.print_ty(fn_sig_tys.output())?;

                    let input_params = input_tys_ast.into_iter()
                        .map(|ty| ast::Param {
                            id: ast::DUMMY_NODE_ID,
                            span: sp,
                            attrs: ast::AttrVec::new(),
                            pat: Box::new(ast::Pat { id: ast::DUMMY_NODE_ID, span: sp, kind: ast::PatKind::Wild }),
                            ty,
                            is_placeholder: false,
                        })
                        .collect();
                    Ok(mk_ty_ast(sp, ast::TyKind::FnPtr(Box::new(ast::FnPtrTy {
                        safety: match fn_header.safety() {
                            hir::Safety::Safe => ast::Safety::Default,
                            hir::Safety::Unsafe => ast::Safety::Unsafe(sp),
                        },
                        ext: ast::Extern::Explicit(ast::StrLit {
                            span: sp,
                            style: ast::StrStyle::Cooked,
                            symbol: Symbol::intern(fn_header.abi().name()),
                            symbol_unescaped: Symbol::intern(fn_header.abi().name()),
                            suffix: None,
                        }, DUMMY_SP),
                        generic_params: ThinVec::new(),
                        decl: Box::new(ast::FnDecl { inputs: input_params, output: ast::FnRetTy::Ty(output_ty_ast) }),
                        decl_span: DUMMY_SP,
                    }))))
                }

                // NOTE: These types cannot be represented directly in the AST, so we must replace them with inference holes.
                ty::TyKind::FnDef(_, _) => Ok(mk_ty_ast(sp, ast::TyKind::Infer)),
                ty::TyKind::Closure(_, _) => Ok(mk_ty_ast(sp, ast::TyKind::Infer)),
                ty::TyKind::Coroutine(_, _) => Ok(mk_ty_ast(sp, ast::TyKind::Infer)),
                ty::TyKind::CoroutineClosure(_, _) => Ok(mk_ty_ast(sp, ast::TyKind::Infer)),
                ty::TyKind::CoroutineWitness(_, _) => Ok(mk_ty_ast(sp, ast::TyKind::Infer)),

                ty::TyKind::Bound(_, _) => Err("encountered bound type variable".to_owned()),
                ty::TyKind::UnsafeBinder(_) => Err("encountered unsafe binder".to_owned()),
                ty::TyKind::Infer(_) => Err("encountered type variable".to_owned()),
                ty::TyKind::Pat(_, _) => Err("encountered pat".to_owned()),

                ty::TyKind::Placeholder(_) => Err("encountered placeholder type".to_owned()),
                ty::TyKind::Error(_) => Err("encountered type error".to_owned()),
            }
        }
    }

    pub fn ty_ast<'tcx>(
        tcx: TyCtxt<'tcx>,
        crate_res: &res::CrateResolutions<'tcx>,
        def_res: &ast_lowering::DefResolutions,
        scope: Option<hir::DefId>,
        sp: Span,
        ty: Ty<'tcx>,
        opaque_ty_handling: OpaqueTyHandling,
        binding_item_def_id: hir::DefId,
    ) -> Option<Box<ast::Ty>> {
        let mut printer = AstTyPrinter {
            tcx,
            crate_res,
            def_res,
            scope,
            sp,
            opaque_ty_handling,
            binding_item_def_id,
        };
        printer.print_ty(ty).ok()
    }

    pub fn region_ast<'tcx>(
        tcx: TyCtxt<'tcx>,
        sp: Span,
        region: ty::Region<'tcx>,
        binding_item_def_id: hir::DefId,
    ) -> Option<ast::Lifetime> {
        // HACK: We construct an AstTyPrinter with some unused dummy values to call the `print_region` impl.
        let mut printer = AstTyPrinter {
            tcx,
            crate_res: &res::CrateResolutions::empty(tcx),
            def_res: &ast_lowering::DefResolutions::empty(),
            scope: None,
            sp,
            opaque_ty_handling: OpaqueTyHandling::Infer,
            binding_item_def_id,
        };
        printer.print_region(region).ok().flatten()
    }

    pub fn const_ast<'tcx>(
        tcx: TyCtxt<'tcx>,
        crate_res: &res::CrateResolutions<'tcx>,
        def_res: &ast_lowering::DefResolutions,
        scope: Option<hir::DefId>,
        sp: Span,
        ct: ty::Const<'tcx>,
        binding_item_def_id: hir::DefId,
    ) -> Option<ast::AnonConst> {
        let mut printer = AstTyPrinter {
            tcx,
            crate_res,
            def_res,
            scope,
            sp,
            opaque_ty_handling: OpaqueTyHandling::Infer,
            binding_item_def_id,
        };
        printer.print_const(ct).ok()
    }
}
