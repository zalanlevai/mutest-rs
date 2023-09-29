use std::iter;

use rustc_hash::FxHashSet;
use rustc_middle::ty::TyCtxt;
use rustc_session::Session;
use thin_vec::{ThinVec, thin_vec};

use crate::analysis::diagnostic;
use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::mut_visit::MutVisitor;
use crate::codegen::expansion::TcxExpansionExt;
use crate::codegen::mutation::{Mut, Mutant, SubstLoc};
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, path, sym};
use crate::codegen::symbols::hygiene::AstPass;

pub fn bake_mutation(mutation: &Mut, sp: Span, sess: &Session) -> P<ast::Expr> {
    ast::mk::expr_struct(sp, ast::mk::path_local(path::MutationMeta(sp)), thin_vec![
        ast::mk::expr_struct_field(sp, Ident::new(*sym::id, sp), {
            ast::mk::expr_u32(sp, mutation.id.index())
        }),

        ast::mk::expr_struct_field(sp, Ident::new(*sym::display_name, sp), {
            ast::mk::expr_str(sp, &mutation.display_name())
        }),
        ast::mk::expr_struct_field(sp, Ident::new(*sym::display_location, sp), {
            ast::mk::expr_str(sp, &mutation.display_location(sess))
        }),

        ast::mk::expr_struct_field(sp, Ident::new(*sym::reachable_from, sp), {
            let args_token_trees = mutation.target.reachable_from.iter()
                .flat_map(|(&test, entry_point)| {
                    let key_lit = ast::TokenKind::lit(ast::token::LitKind::Str, Symbol::intern(&test.path_str()), None);
                    let key_token = ast::mk::tt_token_alone(sp, key_lit);

                    let arrow_token = ast::mk::tt_token_alone(sp, ast::TokenKind::FatArrow);

                    let value_lit = ast::TokenKind::lit(ast::token::LitKind::Integer, Symbol::intern(&entry_point.distance.to_string()), None);
                    let value_token = ast::mk::tt_token_alone(sp, value_lit);

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
        }),

        ast::mk::expr_struct_field(sp, Ident::new(*sym::undetected_diagnostic, sp), {
            ast::mk::expr_str(sp, &diagnostic::escape_literal(&mutation.undetected_diagnostic(sess)))
        }),
    ])
}

pub fn bake_mutant(_mutant: &Mutant, sp: Span, _sess: &Session, mutations_expr: P<ast::Expr>, subst_map_expr: P<ast::Expr>) -> P<ast::Expr> {
    ast::mk::expr_struct(sp, ast::mk::path_local(path::MutantMeta(sp)), thin_vec![
        ast::mk::expr_struct_field(sp, Ident::new(*sym::mutations, sp), mutations_expr),
        ast::mk::expr_struct_field(sp, Ident::new(*sym::substitutions, sp), subst_map_expr),
    ])
}

fn mk_subst_map_struct(sp: Span, subst_locs: &Vec<SubstLoc>) -> P<ast::Item> {
    let fields = subst_locs.iter()
        .map(|subst_loc| {
            // pub $subst_loc_id: Option<SubstMeta>,
            let vis = ast::mk::vis_pub(sp);
            let ident = Ident::new(subst_loc.into_subst_loc_id().into_symbol(), sp);
            let ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp, path::Option(sp), vec![], vec![
                ast::GenericArg::Type(ast::mk::ty_path(None, ast::mk::path_local(path::SubstMeta(sp)))),
            ]));
            ast::mk::field_def(sp, vis, Some(ident), ty)
        })
        .collect::<ThinVec<_>>();

    // pub(crate) struct SubstMap { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(*sym::SubstMap, sp);
    ast::mk::item_struct(sp, vis, ident, None, fields)
}

fn mk_mutations_mod(sp: Span, sess: &Session, mutations: &Vec<&Mut>) -> P<ast::Item> {
    let g = &sess.parse_sess.attr_id_generator;

    let items = iter::once(ast::mk::item_extern_crate(sp, *sym::mutest_runtime, None))
        .chain(mutations.iter().map(|mutation| {
            // pub const $mut_id: MutationMeta = MutationMeta { ... };
            let vis = ast::mk::vis_pub(sp);
            let ident = Ident::new(mutation.id.into_symbol(), sp);
            let ty = ast::mk::ty_path(None, ast::mk::path_local(path::MutationMeta(sp)));
            let expr = bake_mutation(mutation, sp, sess);
            ast::mk::item_const(sp, vis, ident, ty, expr)
        }))
        .collect::<ThinVec<_>>();

    // #[allow(non_upper_case_globals)]
    let allow_non_upper_case_globals_attr = ast::mk::attr_outer(g, sp,
        Ident::new(sym::allow, sp),
        ast::mk::attr_args_delimited(sp, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
            ast::mk::tt_token_joint(sp, ast::TokenKind::Ident(*sym::non_upper_case_globals, false)),
        ])),
    );

    // pub(crate) mod mutations { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(*sym::mutations, sp);
    ast::mk::item_mod(sp, vis, ident, items).map(|mut m| { m.attrs = thin_vec![allow_non_upper_case_globals_attr]; m })
}

fn mk_mutants_slice_const(sp: Span, sess: &Session, mutants: &[Mutant], subst_locs: &Vec<SubstLoc>) -> P<ast::Item> {
    let elements = mutants.iter()
        .map(|mutant| {
            // &[...]
            let elements = mutant.mutations.iter()
                .map(|mutation| {
                    // &mutations::$mut_id
                    ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp,
                        path::mutations(sp),
                        vec![Ident::new(mutation.id.into_symbol(), sp)],
                    )))
                })
                .collect::<ThinVec<_>>();
            let mutations_expr = ast::mk::expr_slice(sp, elements);

            // SubstMap { ... }
            let subst_map_fields = subst_locs.iter()
                .map(|subst_loc| {
                    // $subst_loc_id: Some(SubstMeta { mutation: &crate::mutest_generated::mutations::$mut_id })
                    let ident = Ident::new(subst_loc.into_subst_loc_id().into_symbol(), sp);
                    let mutation = mutant.mutations.iter().find(|m| m.substs.iter().any(|s| s.location == *subst_loc));
                    let expr = match mutation {
                        Some(mutation) => {
                            let subst_meta_struct_expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::SubstMeta(sp)), thin_vec![
                                ast::mk::expr_struct_field(sp, Ident::new(*sym::mutation, sp), {
                                    let mutation_path = ast::mk::pathx(sp,
                                        path::mutations(sp),
                                        vec![Ident::new(mutation.id.into_symbol(), sp)],
                                    );
                                    ast::mk::expr_ref(sp, ast::mk::expr_path(mutation_path))
                                }),
                            ]);

                            ast::mk::expr_call_path(sp, path::Some(sp), thin_vec![subst_meta_struct_expr])
                        },
                        None => ast::mk::expr_path(path::None(sp)),
                    };
                    ast::mk::expr_struct_field(sp, ident, expr)
                })
                .collect::<ThinVec<_>>();
            let subst_map_expr = ast::mk::expr_struct(sp, path::SubstMap(sp), subst_map_fields);

            // &MutantMeta { ... }
            ast::mk::expr_ref(sp, bake_mutant(mutant, sp, sess, mutations_expr, subst_map_expr))
        })
        .collect::<ThinVec<_>>();

    // const MUTANTS: : &[&mutest_runtime::MutantMeta<SubstMap>] = &[ ... ];
    let vis = ast::mk::vis_default(sp);
    let ident = Ident::new(*sym::MUTANTS, sp);
    let mutant_meta_ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp,
        ast::mk::path_local(path::MutantMeta(sp)),
        vec![],
        vec![ast::GenericArg::Type(ast::mk::ty_path(None, path::SubstMap(sp)))],
    ));
    let ty = ast::mk::ty_ref(sp, ast::mk::ty_slice(sp, ast::mk::ty_ref(sp, mutant_meta_ty, None)), None);
    let expr = ast::mk::expr_slice(sp, elements);
    ast::mk::item_const(sp, vis, ident, ty, expr)
}

fn mk_active_mutant_handle_static(sp: Span) -> P<ast::Item> {
    // pub(crate) static ACTIVE_MUTANT_HANDLE: ActiveMutantHandle<Mutant> = ActiveMutantHandle::empty();
    let vis = ast::mk::vis_pub_crate(sp);
    let mutbl = ast::Mutability::Not;
    let ident = Ident::new(*sym::ACTIVE_MUTANT_HANDLE, sp);
    let ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp,
        ast::mk::path_local(path::ActiveMutantHandle(sp)),
        vec![],
        vec![ast::GenericArg::Type(ast::mk::ty_path(None, ast::mk::path_local(path::SubstMap(sp))))],
    ));
    let expr = ast::mk::expr_call(sp, ast::mk::expr_path(ast::mk::path_local(path::active_mutant_handle_init_empty(sp))), ThinVec::new());
    ast::mk::item_static(sp, vis, mutbl, ident, ty, expr)
}

fn mk_harness_fn(sp: Span) -> P<ast::Item> {
    // mutest_runtime::mutest_main_static(...);
    let test_runner = ast::mk::expr_path(ast::mk::path_local(path::mutest_main_static(sp)));
    let call_test_main = ast::mk::stmt_expr(ast::mk::expr_call(sp, test_runner, thin_vec![
        ast::mk::expr_ident(sp, Ident::new(*sym::tests, sp)),
        ast::mk::expr_path(path::MUTANTS(sp)),
        ast::mk::expr_ref(sp, ast::mk::expr_path(path::ACTIVE_MUTANT_HANDLE(sp))),
    ]));

    let body = ast::mk::block(sp, thin_vec![call_test_main]);

    // pub(crate) fn harness(tests: &[&test::TestDescAndFn]) { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(*sym::harness, sp);
    let inputs = thin_vec![ast::mk::param_ident(sp, Ident::new(*sym::tests, sp), {
        // &[&test::TestDescAndFn]
        let element_ty = ast::mk::ty_ref(sp, ast::mk::ty_path(None, ast::mk::path_local(path::TestDescAndFn(sp))), None);
        ast::mk::ty_ref(sp, ast::mk::ty_slice(sp, element_ty), None)
    })];
    ast::mk::item_fn(sp, vis, ident, None, None, inputs, None, Some(body))
}

struct HarnessGenerator<'tcx, 'trg, 'm> {
    sess: &'tcx Session,
    mutants: &'m [Mutant<'tcx, 'trg, 'm>],
    def_site: Span,
}

impl<'tcx, 'trg, 'm> ast::mut_visit::MutVisitor for HarnessGenerator<'tcx, 'trg, 'm> {
    fn visit_crate(&mut self, c: &mut ast::Crate) {
        ast::mut_visit::noop_visit_crate(c, self);

        let g = &self.sess.parse_sess.attr_id_generator;

        let def = self.def_site;

        let mutations = FxHashSet::from_iter(self.mutants.iter().flat_map(|m| &m.mutations)).into_iter().collect::<Vec<_>>();
        let subst_locs = FxHashSet::from_iter(self.mutants.iter().flat_map(|m| m.iter_substitutions().map(|s| s.location))).into_iter().collect::<Vec<_>>();

        // #![feature(test)]
        let feature_test_attr = ast::mk::attr_inner(g, def,
            Ident::new(sym::feature, def),
            ast::mk::attr_args_delimited(def, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                ast::mk::tt_token_joint(def, ast::TokenKind::Ident(sym::test, false)),
            ])),
        );
        // #![feature(custom_test_frameworks)]
        let feature_custom_test_frameworks_attr = ast::mk::attr_inner(g, def,
            Ident::new(sym::feature, def),
            ast::mk::attr_args_delimited(def, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                ast::mk::tt_token_joint(def, ast::TokenKind::Ident(sym::custom_test_frameworks, false)),
            ])),
        );
        // #![test_runner(mutest_generated::harness)]
        let test_runner_mutest_harness_attr = ast::mk::attr_inner(g, def,
            Ident::new(sym::test_runner, def),
            ast::mk::attr_args_delimited(def, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(
                ast::mk::ts_path(def, ast::mk::path_local(path::harness(def))),
            )),
        );

        c.attrs.push(feature_test_attr);
        c.attrs.push(feature_custom_test_frameworks_attr);
        c.attrs.push(test_runner_mutest_harness_attr);

        // extern crate test;
        let extern_crate_test = ast::mk::item_extern_crate(def, sym::test, None);
        // extern crate mutest_runtime;
        let extern_crate_mutest_runtime = ast::mk::item_extern_crate(def, *sym::mutest_runtime, None);

        // pub(crate) mod mutest_generated { ... }
        let mutest_generated_mod = ast::mk::item_mod(def,
            ast::mk::vis_pub_crate(def),
            Ident::new(*sym::mutest_generated, def),
            thin_vec![
                extern_crate_test,
                extern_crate_mutest_runtime,
                mk_subst_map_struct(def, &subst_locs),
                mk_mutations_mod(def, self.sess, &mutations),
                mk_mutants_slice_const(def, self.sess, self.mutants, &subst_locs),
                mk_active_mutant_handle_static(def),
                mk_harness_fn(def),
            ],
        );

        c.items.push(mutest_generated_mod);
    }
}

pub fn generate_harness<'tcx>(tcx: TyCtxt<'tcx>, mutants: &Vec<Mutant>, krate: &mut ast::Crate) {
    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::test, sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut generator = HarnessGenerator { sess: tcx.sess, mutants, def_site };
    generator.visit_crate(krate);
}
