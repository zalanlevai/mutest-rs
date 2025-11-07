use std::iter;

use rustc_middle::bug;
use rustc_middle::ty::TyCtxt;
use thin_vec::{ThinVec, thin_vec};

use crate::analysis::call_graph::{EntryPoints, TargetReachability, Unsafety};
use crate::analysis::diagnostic;
use crate::analysis::hir;
use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::expansion::TcxExpansionExt;
use crate::codegen::mutation::{Mut, MutationBatch, MutationParallelism, SubstLoc, UnsafeTargeting};
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, kw, path, sym};
use crate::codegen::symbols::hygiene::AstPass;

/// Generate an opaque identifier representing a crate definintion
/// that is stable across crate (and compilation session) boundaries.
/// This means that the generated code can rely on equivalence between
/// two opaque stable def ids to determine references to the same crate definition.
/// The identifier is returned in the form of a [`Symbol`] for embedding into generated code.
///
/// The identifier is based on the definition's [`DefPathHash`][hir::DefPathHash].
pub fn opaque_stable_def_id_for_embedding<'tcx>(tcx: TyCtxt<'tcx>, prefix: &str, def_id: hir::DefId) -> Symbol {
    Symbol::intern(&format!("{prefix}{}", tcx.def_path_hash(def_id).0.to_hex()))
}

pub fn mk_crate_kind_const(sp: Span, crate_kind: &str) -> P<ast::Item> {
    // pub const CRATE_KIND: &str = "...";
    let vis = ast::mk::vis_pub(sp);
    let ident = Ident::new(sym::CRATE_KIND, sp);
    let ty = ast::mk::ty_ref(sp, ast::mk::ty_ident(sp, None, Ident::new(sym::str, sp)), None);
    let expr = ast::mk::expr_str(sp, crate_kind);
    ast::mk::item_const(sp, vis, ident, ty, expr)
}

fn mk_static_map<I>(sp: Span, entries: I) -> P<ast::Expr>
where
    I: IntoIterator<Item = (ast::tokenstream::TokenTree, ast::tokenstream::TokenTree)>,
{
    let args_token_trees = entries.into_iter()
        .flat_map(|(key_token, value_token)| {
            let arrow_token = ast::mk::tt_token_alone(sp, ast::TokenKind::FatArrow);
            let comma_token = ast::mk::tt_token_alone(sp, ast::TokenKind::Comma);
            [key_token, arrow_token, value_token, comma_token]
        })
        .collect::<Vec<_>>();

    ast::mk::expr(sp, ast::ExprKind::MacCall(P(ast::MacCall {
        path: ast::mk::path_local(path::static_map(sp)),
        args: P(ast::DelimArgs {
            dspan: ast::tokenstream::DelimSpan::from_single(sp),
            delim: ast::token::Delimiter::Brace,
            tokens: ast::mk::token_stream(args_token_trees),
        })
    })))
}

pub fn bake_mutation<'tcx, 'ent>(sp: Span, tcx: TyCtxt<'tcx>, entry_points: EntryPoints<'ent>, mutation: &Mut, unsafe_targeting: UnsafeTargeting) -> P<ast::Expr> {
    ast::mk::expr_struct(sp, ast::mk::path_local(path::MutationMeta(sp)), thin_vec![
        ast::mk::expr_struct_field(sp, Ident::new(sym::id, sp), {
            ast::mk::expr_u32(sp, mutation.id.index())
        }),

        ast::mk::expr_struct_field(sp, Ident::new(sym::safety, sp), {
            match (mutation.is_unsafe(unsafe_targeting), mutation.target.unsafety) {
                (true, Unsafety::Tainted(_)) => ast::mk::expr_path(ast::mk::path_local(path::MutationSafetyTainted(sp))),
                (true, _) => ast::mk::expr_path(ast::mk::path_local(path::MutationSafetyUnsafe(sp))),
                (false, _) => ast::mk::expr_path(ast::mk::path_local(path::MutationSafetySafe(sp))),
            }
        }),

        ast::mk::expr_struct_field(sp, Ident::new(sym::op_name, sp), {
            ast::mk::expr_str(sp, &mutation.op_name())
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::display_name, sp), {
            ast::mk::expr_str(sp, &mutation.display_name())
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::display_location, sp), {
            ast::mk::expr_str(sp, &diagnostic::escape_literal(&mutation.display_location(tcx.sess)))
        }),

        ast::mk::expr_struct_field(sp, Ident::new(sym::reachable_from, sp), {
            // FIXME: Decide whether we want to support this, or remove it instead.
            if let TargetReachability::DirectEntry = mutation.target.reachability {
                bug!("encountered mutation target that is also an entry point");
            }

            let map = mk_static_map(sp, mutation.target.reachable_from.iter().map(|(entry_point, entry_point_assoc)| {
                let key_lit = ast::TokenKind::lit(ast::token::LitKind::Str, Symbol::intern(&entry_point.path_str(tcx)), None);
                let key_token = ast::mk::tt_token_alone(sp, key_lit);

                let value_lit = ast::TokenKind::lit(ast::token::LitKind::Integer, Symbol::intern(&entry_point_assoc.distance.to_string()), None);
                let value_token = ast::mk::tt_token_alone(sp, value_lit);

                (key_token, value_token)
            }));

            match entry_points {
                EntryPoints::Tests(_) => ast::mk::expr_call_path(sp, ast::mk::path_local(path::EntryPointsInternalTests(sp)), thin_vec![map]),
                EntryPoints::External => ast::mk::expr_call_path(sp, ast::mk::path_local(path::EntryPointsExternalTests(sp)), thin_vec![map]),
            }
        }),

        ast::mk::expr_struct_field(sp, Ident::new(sym::undetected_diagnostic, sp), {
            ast::mk::expr_str(sp, &diagnostic::escape_literal(&mutation.undetected_diagnostic(tcx.sess)))
        }),
    ])
}

fn mk_mutations_mod<'tcx, 'ent, 'trg, 'm>(sp: Span, tcx: TyCtxt<'tcx>, entry_points: EntryPoints<'ent>, mutations: &'m [Mut<'trg, 'm>], unsafe_targeting: UnsafeTargeting) -> P<ast::Item> {
    let g = &tcx.sess.psess.attr_id_generator;

    let items = iter::once(ast::mk::item_extern_crate(sp, sym::mutest_runtime, None))
        .chain(mutations.iter().map(|mutation| {
            // pub const $mut_id: mutest_runtime::MutationMeta = mutest_runtime::MutationMeta { ... };
            let vis = ast::mk::vis_pub(sp);
            let ident = Ident::new(mutation.id.into_symbol(), sp);
            let ty = ast::mk::ty_path(None, ast::mk::path_local(path::MutationMeta(sp)));
            let expr = bake_mutation(sp, tcx, entry_points, mutation, unsafe_targeting);
            ast::mk::item_const(sp, vis, ident, ty, expr)
        }))
        .collect::<ThinVec<_>>();

    // #[allow(non_upper_case_globals)]
    let allow_non_upper_case_globals_attr = ast::mk::attr_outer(g, sp,
        ast::Safety::Default,
        Ident::new(sym::allow, sp),
        ast::mk::attr_args_delimited(sp, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
            ast::mk::tt_token_joint(sp, ast::TokenKind::Ident(sym::non_upper_case_globals, ast::token::IdentIsRaw::No)),
        ])),
    );

    // pub(crate) mod mutations { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(sym::mutations, sp);
    ast::mk::item_mod(sp, vis, ident, items).map(|mut m| { m.attrs = thin_vec![allow_non_upper_case_globals_attr]; m })
}

pub enum Mutant<'trg, 'm> {
    Mutation(&'m Mut<'trg, 'm>),
    Batch(&'m MutationBatch<'trg, 'm>),
}

pub fn bake_mutant<'trg, 'm>(sp: Span, mutant: Mutant<'trg, 'm>, subst_locs: &[SubstLoc]) -> P<ast::Expr> {
    let mutations = match mutant {
        Mutant::Mutation(mutation) => &[mutation],
        Mutant::Batch(mutation_batch) => &mutation_batch.mutations[..],
    };

    let subst_map_expr = {
        let subst_map_entries = subst_locs.iter().enumerate()
            .filter_map(|(subst_loc_idx, subst_loc)| {
                let mutation = mutations.iter().find(|m| m.substs.iter().any(|s| s.location == *subst_loc))?;
                Some((subst_loc_idx, subst_loc, mutation))
            })
            .map(|(subst_loc_idx, _subst_loc, mutation)| {
                let subst_loc_idx_expr = ast::mk::expr_lit(sp, ast::token::LitKind::Integer, Symbol::intern(&subst_loc_idx.to_string()), None);

                // SubstMeta { mutation: &crate::mutest_generated::mutations::$mut_id }
                let subst_meta_struct_expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::SubstMeta(sp)), thin_vec![
                    ast::mk::expr_struct_field(sp, Ident::new(sym::mutation, sp), {
                        // &mutations::$mut_id
                        ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp,
                            path::mutations(sp),
                            vec![Ident::new(mutation.id.into_symbol(), sp)],
                        )))
                    }),
                ]);

                // ($subst_loc_idx, SubstMeta { mutation: &crate::mutest_generated::mutations::$mut_id })
                ast::mk::expr_tuple(sp, thin_vec![subst_loc_idx_expr, subst_meta_struct_expr])
            })
            .collect::<ThinVec<_>>();

        ast::mk::expr_ref(sp, ast::mk::expr_call_path(sp, ast::mk::path_local(path::subst_map_array(sp)), thin_vec![
            ast::mk::expr_slice(sp, subst_map_entries),
        ]))
    };

    match mutant {
        Mutant::Mutation(mutation) => {
            ast::mk::expr_struct(sp, ast::mk::path_local(path::StandaloneMutantMeta(sp)), thin_vec![
                ast::mk::expr_struct_field(sp, Ident::new(sym::mutation, sp), {
                    // &mutations::$mut_id
                    ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp,
                        path::mutations(sp),
                        vec![Ident::new(mutation.id.into_symbol(), sp)],
                    )))
                }),
                ast::mk::expr_struct_field(sp, Ident::new(sym::substitutions, sp), subst_map_expr),
            ])
        }
        Mutant::Batch(mutation_batch) => {
            ast::mk::expr_struct(sp, ast::mk::path_local(path::BatchedMutantMeta(sp)), thin_vec![
                ast::mk::expr_struct_field(sp, Ident::new(sym::batch_id, sp), {
                    ast::mk::expr_u32(sp, mutation_batch.id.index())
                }),
                ast::mk::expr_struct_field(sp, Ident::new(sym::mutations, sp), {
                    let elements = mutation_batch.mutations.iter()
                        .map(|mutation| {
                            // &mutations::$mut_id
                            ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp,
                                path::mutations(sp),
                                vec![Ident::new(mutation.id.into_symbol(), sp)],
                            )))
                        })
                        .collect::<ThinVec<_>>();
                    // &[&mutations::$mut_id, ..]
                    ast::mk::expr_slice(sp, elements)
                }),
                ast::mk::expr_struct_field(sp, Ident::new(sym::substitutions, sp), subst_map_expr),
            ])
        }
    }
}

fn mk_mutants_slice_const<'trg, 'm>(sp: Span, mutations: &'m [Mut<'trg, 'm>], mutation_parallelism: Option<MutationParallelism<'trg, 'm>>, subst_locs: &[SubstLoc]) -> P<ast::Item> {
    let (mutant_meta_ty, mutants) = match mutation_parallelism {
        None => {
            // mutest_runtime::StandaloneMutantMeta<SubstMap>
            let mutant_meta_ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp,
                ast::mk::path_local(path::StandaloneMutantMeta(sp)),
                vec![],
                vec![ast::GenericArg::Type(ast::mk::ty_path(None, path::SubstMap(sp)))],
            ));

            let mutants = mutations.iter()
                .map(|mutation| bake_mutant(sp, Mutant::Mutation(mutation), subst_locs))
                .collect::<ThinVec<_>>();

            (mutant_meta_ty, mutants)
        }

        Some(MutationParallelism::Batched(mutation_batches)) => {
            // mutest_runtime::BatchedMutantMeta<SubstMap>
            let mutant_meta_ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp,
                ast::mk::path_local(path::BatchedMutantMeta(sp)),
                vec![],
                vec![ast::GenericArg::Type(ast::mk::ty_path(None, path::SubstMap(sp)))],
            ));

            let mutants = mutation_batches.iter()
                .map(|mutation_batch| bake_mutant(sp, Mutant::Batch(mutation_batch), subst_locs))
                .collect::<ThinVec<_>>();

            (mutant_meta_ty, mutants)
        }
    };

    // const MUTANTS: &[$mutant_meta_ty] = &[ ... ];
    let vis = ast::mk::vis_default(sp);
    let ident = Ident::new(sym::MUTANTS, sp);
    let ty = ast::mk::ty_ref(sp, ast::mk::ty_slice(sp, mutant_meta_ty), None);
    let expr = ast::mk::expr_slice(sp, mutants);
    ast::mk::item_const(sp, vis, ident, ty, expr)
}

fn mk_subst_map_ty_alias(sp: Span, subst_locs: &[SubstLoc]) -> P<ast::Item> {
    let option_subst_meta_ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp, path::Option(sp), vec![], vec![
        ast::GenericArg::Type(ast::mk::ty_path(None, ast::mk::path_local(path::SubstMeta(sp)))),
    ]));

    let subst_locs_count_anon_const = ast::mk::anon_const(sp, ast::mk::expr_lit(sp, ast::token::LitKind::Integer, Symbol::intern(&subst_locs.len().to_string()), None).into_inner().kind);

    // pub type SubstMap = [Option<mutest_runtime::SubstMeta>; $subst_locs_count];
    let vis = ast::mk::vis_pub(sp);
    let ident = Ident::new(sym::SubstMap, sp);
    ast::mk::item(sp, thin_vec![], vis, ast::ItemKind::TyAlias(Box::new(ast::TyAlias {
        ident,
        defaultness: ast::Defaultness::Final,
        generics: Default::default(),
        where_clauses: Default::default(),
        bounds: vec![],
        // [Option<mutest_runtime::SubstMeta>; $subst_locs_count]
        ty: Some(ast::mk::ty_array(sp, option_subst_meta_ty, subst_locs_count_anon_const)),
    })))
}

fn mk_active_mutant_handle_static(sp: Span) -> P<ast::Item> {
    // pub(crate) static ACTIVE_MUTANT_HANDLE: ActiveMutantHandle<Mutant> = ActiveMutantHandle::empty();
    let vis = ast::mk::vis_pub_crate(sp);
    let mutbl = ast::Mutability::Not;
    let ident = Ident::new(sym::ACTIVE_MUTANT_HANDLE, sp);
    let ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp,
        ast::mk::path_local(path::ActiveMutantHandle(sp)),
        vec![],
        vec![ast::GenericArg::Type(ast::mk::ty_path(None, ast::mk::path_local(path::SubstMap(sp))))],
    ));
    let expr = ast::mk::expr_call(sp, ast::mk::expr_path(ast::mk::path_local(path::active_mutant_handle_init_empty(sp))), ThinVec::new());
    ast::mk::item_static(sp, vis, mutbl, ident, ty, expr)
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum CargoTargetKind {
    Lib,
    MainBin,
    Bin,
    Example,
    Test,
}

#[derive(Debug)]
pub struct CargoMetadata {
    pub package_name: String,
    pub target_kind: CargoTargetKind,
}

fn mk_meta_mutant_struct_static<'tcx, 'trg, 'm>(sp: Span, tcx: TyCtxt<'tcx>, cargo_metadata: Option<&CargoMetadata>, mutations: &'m [Mut<'trg, 'm>], mutation_parallelism: Option<MutationParallelism<'trg, 'm>>) -> P<ast::Item> {
    // mutest_runtime::MetaMutant { ... }
    let meta_mutant_struct_expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::MetaMutant(sp)), thin_vec![
        ast::mk::expr_struct_field(sp, Ident::new(sym::cargo_package_name, sp), {
            match cargo_metadata {
                Some(cargo_metadata) => {
                    let cargo_package_name_str = ast::mk::expr_str(sp, &cargo_metadata.package_name);
                    ast::mk::expr_call_path(sp, path::Some(sp), thin_vec![cargo_package_name_str])
                }
                None => ast::mk::expr_path(path::None(sp)),
            }
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::cargo_target_kind, sp), {
            match cargo_metadata {
                Some(cargo_metadata) => {
                    let cargo_metadata_expr = match cargo_metadata.target_kind {
                        CargoTargetKind::Lib => ast::mk::expr_path(ast::mk::path_local(path::CargoTargetKindLib(sp))),
                        CargoTargetKind::MainBin => ast::mk::expr_path(ast::mk::path_local(path::CargoTargetKindMainBin(sp))),
                        CargoTargetKind::Bin => ast::mk::expr_path(ast::mk::path_local(path::CargoTargetKindBin(sp))),
                        CargoTargetKind::Example => ast::mk::expr_path(ast::mk::path_local(path::CargoTargetKindExample(sp))),
                        CargoTargetKind::Test => ast::mk::expr_path(ast::mk::path_local(path::CargoTargetKindTest(sp))),
                    };
                    ast::mk::expr_call_path(sp, path::Some(sp), thin_vec![cargo_metadata_expr])
                }
                None => ast::mk::expr_path(path::None(sp)),
            }
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::crate_name, sp), {
            ast::mk::expr_str(sp, tcx.crate_name(hir::LOCAL_CRATE).as_str())
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::active_mutant_handle, sp), {
            ast::mk::expr_ref(sp, ast::mk::expr_path(path::ACTIVE_MUTANT_HANDLE(sp)))
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::mutations, sp), {
            let elements = mutations.iter()
                .map(|mutation| {
                    // &mutations::$mut_id
                    ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp,
                        path::mutations(sp),
                        vec![Ident::new(mutation.id.into_symbol(), sp)],
                    )))
                })
                .collect::<ThinVec<_>>();
            // &[&mutations::mut_1, &mutations::mut_2, ..]
            ast::mk::expr_slice(sp, elements)
        }),
        ast::mk::expr_struct_field(sp, Ident::new(sym::mutation_parallelism, sp), {
            match mutation_parallelism {
                None => {
                    ast::mk::expr_call_path(sp, ast::mk::path_local(path::MutationParallelismNone(sp)), thin_vec![
                        ast::mk::expr_path(path::MUTANTS(sp)),
                    ])
                }
                Some(MutationParallelism::Batched(_mutation_batches)) => {
                    ast::mk::expr_call_path(sp, ast::mk::path_local(path::MutationParallelismBatched(sp)), thin_vec![
                        ast::mk::expr_path(path::MUTANTS(sp)),
                    ])
                }
            }
        }),
    ]);

    // NOTE: Because `META_MUTANT` stores a reference to the `ACTIVE_MUTANT_HANDLE` static, it must also be a static:
    //       error[E0080]: constructing invalid value at .active_mutant_handle: encountered reference to mutable memory in `const`
    // pub static META_MUTANT: mutest_runtime::MetaMutant<SubstMap> = mutest_runtime::MetaMutant { ... };
    let vis = ast::mk::vis_pub(sp);
    let mutbl = ast::Mutability::Not;
    let ident = Ident::new(sym::META_MUTANT, sp);
    let ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp,
        ast::mk::path_local(path::MetaMutant(sp)),
        vec![],
        vec![ast::GenericArg::Type(ast::mk::ty_path(None, path::SubstMap(sp)))],
    ));
    ast::mk::item_static(sp, vis, mutbl, ident, ty, meta_mutant_struct_expr)
}

fn mk_external_tests_extra_const<'tcx, 'trg>(sp: Span, tcx: TyCtxt<'tcx>) -> P<ast::Item> {
    // mutest_runtime::ExternalTestsExtra { .. }
    let external_tests_extra_struct_expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::ExternalTestsExtra(sp)), thin_vec![
        ast::mk::expr_struct_field(sp, Ident::new(sym::test_crate_name, sp), {
            ast::mk::expr_str(sp, tcx.crate_name(hir::LOCAL_CRATE).as_str())
        }),
    ]);

    // const EXTERNAL_TESTS_EXTRA: mutest_runtime::ExternalTestsExtra = mutest_runtime::ExternalTestsExtra { .. };
    let vis = ast::mk::vis_default(sp);
    let ident = Ident::new(sym::EXTERNAL_TESTS_EXTRA, sp);
    let ty = ast::mk::ty_path(None, ast::mk::path_local(path::ExternalTestsExtra(sp)));
    let expr = external_tests_extra_struct_expr;
    ast::mk::item_const(sp, vis, ident, ty, expr)
}

fn mk_harness_fn(sp: Span, embedded: bool, external_meta_mutant: Option<Symbol>) -> P<ast::Item> {
    let meta_mutant_path_expr = match external_meta_mutant {
        None => ast::mk::expr_path(path::META_MUTANT(sp)),
        Some(crate_name) => ast::mk::expr_path(ast::mk::path_global(sp, vec![Ident::new(crate_name, sp), Ident::new(sym::mutest_generated, sp), Ident::new(sym::META_MUTANT, sp)])),
    };

    let external_tests_extra_expr = match external_meta_mutant {
        Some(_) => {
            let external_tests_extra_path_expr = ast::mk::expr_path(ast::mk::path_local(path::EXTERNAL_TESTS_EXTRA(sp)));
            ast::mk::expr_call_path(sp, path::Some(sp), thin_vec![ast::mk::expr_ref(sp, external_tests_extra_path_expr)])
        }
        None => ast::mk::expr_path(path::None(sp)),
    };

    // mutest_runtime::mutest_main_static(...);
    let test_runner = ast::mk::expr_path(ast::mk::path_local(path::mutest_main_static(sp)));
    let call_test_main = ast::mk::stmt_expr(ast::mk::expr_call(sp, test_runner, thin_vec![
        ast::mk::expr_call_path(sp, ast::mk::path_local(path::TestSuiteTests(sp)), thin_vec![
            ast::mk::expr_ident(sp, Ident::new(sym::tests, sp)),
            external_tests_extra_expr,
        ]),
        ast::mk::expr_ref(sp, meta_mutant_path_expr),
    ]));

    let body = ast::mk::block(sp, thin_vec![call_test_main]);

    // pub(crate) fn harness(tests: &'static [&'static test::TestDescAndFn]) { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(sym::harness, sp);
    let inputs = thin_vec![ast::mk::param_ident(sp, Ident::new(sym::tests, sp), {
        let static_lifetime = ast::mk::lifetime(sp, Ident::new(kw::StaticLifetime, sp));

        // &'static [...]
        let element_ty = match embedded {
            // &'static test::TestDescAndFn
            false => ast::mk::ty_ref(sp, ast::mk::ty_path(None, ast::mk::path_local(path::TestDescAndFn(sp))), Some(static_lifetime)),
            // &'static mutest_runtime::EmbeddedTestDescAndFn
            true => ast::mk::ty_ref(sp, ast::mk::ty_path(None, ast::mk::path_local(path::EmbeddedTestDescAndFn(sp))), Some(static_lifetime)),
        };
        ast::mk::ty_ref(sp, ast::mk::ty_slice(sp, element_ty), Some(static_lifetime))
    })];
    ast::mk::item_fn(sp, vis, ident, None, None, inputs, None, Some(body))
}

#[derive(Copy, Clone)]
pub enum MetaMutant<'trg, 'm> {
    Internal {
        mutations: &'m [Mut<'trg, 'm>],
        subst_locs: &'m [SubstLoc],
        mutation_parallelism: Option<MutationParallelism<'trg, 'm>>,
        unsafe_targeting: UnsafeTargeting,
    },
    External {
        crate_name: Symbol,
    },
}

pub fn generate_harness<'tcx, 'ent, 'trg, 'm>(
    tcx: TyCtxt<'tcx>,
    cargo_metadata: Option<&CargoMetadata>,
    embedded: bool,
    entry_points: EntryPoints<'ent>,
    meta_mutant: MetaMutant<'trg, 'm>,
    krate: &mut ast::Crate,
) {
    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::test, sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let g = &tcx.sess.psess.attr_id_generator;

    // #![feature(test)]
    if !krate.attrs.iter().any(|attr| ast::inspect::is_list_attr_with_ident(attr, None, sym::feature, sym::test)) {
        let feature_test_attr = ast::mk::attr_inner(g, def_site,
            Ident::new(sym::feature, def_site),
            ast::mk::attr_args_delimited(def_site, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                ast::mk::tt_token_joint(def_site, ast::TokenKind::Ident(sym::test, ast::token::IdentIsRaw::No)),
            ])),
        );
        krate.attrs.push(feature_test_attr);
    }
    // #![feature(custom_test_frameworks)]
    if !krate.attrs.iter().any(|attr| ast::inspect::is_list_attr_with_ident(attr, None, sym::feature, sym::custom_test_frameworks)) {
        let feature_custom_test_frameworks_attr = ast::mk::attr_inner(g, def_site,
            Ident::new(sym::feature, def_site),
            ast::mk::attr_args_delimited(def_site, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                ast::mk::tt_token_joint(def_site, ast::TokenKind::Ident(sym::custom_test_frameworks, ast::token::IdentIsRaw::No)),
            ])),
        );
        krate.attrs.push(feature_custom_test_frameworks_attr);
    }
    // #![test_runner(mutest_generated::harness)]
    krate.attrs.retain(|attr| !ast::inspect::is_list_attr_with_some(attr, None, sym::test_runner));
    let test_runner_mutest_harness_attr = ast::mk::attr_inner(g, def_site,
        Ident::new(sym::test_runner, def_site),
        ast::mk::attr_args_delimited(def_site, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(
            ast::mk::ts_path(def_site, path::harness(def_site)),
        )),
    );
    krate.attrs.push(test_runner_mutest_harness_attr);

    if let Some(existing_item) = tcx.module_children_local(hir::CRATE_DEF_ID).iter().find(|mod_child| mod_child.ident.name == sym::mutest_generated && mod_child.res.ns() == Some(hir::Namespace::TypeNS)) {
        let mut diagnostic = tcx.dcx().struct_fatal(format!("mutest-injected module conflicts with existing item `{}`", tcx.def_path_str(existing_item.res.def_id())));
        diagnostic.note(format!("mutest injects a module into the crate root with the reserved name `{}`", sym::mutest_generated));
        diagnostic.emit();
    }

    let mut mutest_generated_mod_items = thin_vec![];

    if !embedded {
        // extern crate test;
        let extern_crate_test = ast::mk::item_extern_crate(def_site, sym::test, None);
        mutest_generated_mod_items.push(extern_crate_test);
    }

    // extern crate mutest_runtime;
    let extern_crate_mutest_runtime = ast::mk::item_extern_crate(def_site, sym::mutest_runtime, None);
    mutest_generated_mod_items.push(extern_crate_mutest_runtime);


    match meta_mutant {
        MetaMutant::Internal { mutations, subst_locs, mutation_parallelism, unsafe_targeting } => {
            mutest_generated_mod_items.extend([
                mk_crate_kind_const(def_site, "meta_mutant"),
                mk_subst_map_ty_alias(def_site, subst_locs),
                mk_active_mutant_handle_static(def_site),
                mk_mutations_mod(def_site, tcx, entry_points, mutations, unsafe_targeting),
                mk_mutants_slice_const(def_site, mutations, mutation_parallelism, subst_locs),
                mk_meta_mutant_struct_static(def_site, tcx, cargo_metadata, mutations, mutation_parallelism),
                mk_harness_fn(def_site, embedded, None),
            ]);
        }
        MetaMutant::External { crate_name } => {
            mutest_generated_mod_items.extend([
                mk_crate_kind_const(def_site, "external_tests"),
                mk_external_tests_extra_const(def_site, tcx),
                mk_harness_fn(def_site, embedded, Some(crate_name)),
            ]);
        }
    }

    // pub mod mutest_generated { ... }
    let mutest_generated_mod = ast::mk::item_mod(def_site,
        ast::mk::vis_pub(def_site),
        Ident::new(sym::mutest_generated, def_site),
        mutest_generated_mod_items,
    );

    krate.items.push(mutest_generated_mod);
}

pub fn find_harness_in_extern_crate<'tcx>(tcx: TyCtxt<'tcx>, cnum: hir::CrateNum) -> Option<hir::DefId> {
    let crate_root_def_id = cnum.as_def_id();
    let crate_root = tcx.module_children(crate_root_def_id);

    crate_root.iter().find_map(|mod_child| {
        if mod_child.ident.name != sym::mutest_generated { return None; }
        if !mod_child.vis.is_public() { return None; }
        mod_child.res.mod_def_id()
    })
}
