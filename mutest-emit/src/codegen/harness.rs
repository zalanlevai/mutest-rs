use std::iter;

use rustc_expand::base::ExtCtxt;
use rustc_hash::FxHashSet;
use rustc_span::hygiene::AstPass;

use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::mut_visit::MutVisitor;
use crate::codegen::mutation::{Mut, Mutant, SubstLoc};
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, kw, path, sym};

fn mk_println(sp: Span, msg: &str) -> ast::Stmt {
    ast::mk::stmt_expr(ast::mk::expr(sp, ast::ExprKind::MacCall(ast::MacCall {
        path: ast::mk::path(sp, false, vec![
            Ident::new(*sym::println, sp),
        ]),
        args: P(ast::MacArgs::Delimited(
            ast::tokenstream::DelimSpan::from_single(sp),
            ast::MacDelimiter::Parenthesis,
            ast::tokenstream::TokenStream::new(vec![
                (
                    ast::tokenstream::TokenTree::Token(ast::token::Token {
                        span: sp,
                        kind: ast::token::TokenKind::Literal(ast::token::Lit {
                            kind: ast::token::LitKind::Str,
                            symbol: Symbol::intern(msg),
                            suffix: None,
                        }),
                    }),
                    ast::tokenstream::Spacing::Alone,
                ),
            ]),
        )),
        prior_type_ascription: None,
    })))
}

fn mk_subst_map_struct(sp: Span, subst_locs: &Vec<SubstLoc>) -> P<ast::Item> {
    let fields = subst_locs.iter()
        .map(|subst_loc| {
            // pub $subst_loc_id: Option<SubstMeta<'static>>,
            let vis = ast::mk::vis_pub(sp);
            let ident = Ident::new(subst_loc.into_subst_loc_id().into_symbol(), sp);
            let ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp, path::Option(sp), vec![], vec![
                ast::GenericArg::Type(ast::mk::ty_path(None, ast::mk::pathx_args(sp, ast::mk::path_local(path::SubstMeta(sp)), vec![], vec![
                    ast::GenericArg::Lifetime(ast::mk::lifetime(sp, Ident::new(kw::StaticLifetime, sp))),
                ]))),
            ]));
            ast::mk::field_def(sp, vis, Some(ident), ty)
        })
        .collect::<Vec<_>>();

    // pub(crate) struct SubstMap { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(*sym::SubstMap, sp);
    ast::mk::item_struct(sp, vis, ident, None, fields)
}

fn mk_mutations_mod(sp: Span, mutations: &Vec<&Mut>) -> P<ast::Item> {
    let items = iter::once(ast::mk::item_extern_crate(sp, *sym::mutest_runtime, None))
        .chain(mutations.iter().map(|mutation| {
            // pub const $mut_id: MutationMeta = MutationMeta {};
            let vis = ast::mk::vis_pub(sp);
            let ident = Ident::new(mutation.id.into_symbol(), sp);
            let ty = ast::mk::ty_path(None, ast::mk::path_local(path::MutationMeta(sp)));
            let expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::MutationMeta(sp)), vec![]);
            ast::mk::item_const(sp, vis, ident, ty, expr)
        }))
        .collect::<Vec<_>>();

    // pub(crate) mod mutations { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(*sym::mutations, sp);
    ast::mk::item_mod(sp, vis, ident, items)
}

fn mk_mutants_slice_const(sp: Span, mutants: &Vec<Mutant>, subst_locs: &Vec<SubstLoc>) -> P<ast::Item> {
    let elements = mutants.iter()
        .map(|mutant| {
            // vec![...]
            // let mutation_element_tokens = mutant.mutations.iter()
            //     .flat_map(|mutation| {
            //         // &mutations::$mut_id
            //         iter::once(ast::mk::ts_token(sp, ast::Spacing::Alone, ast::TokenKind::BinOp(ast::token::BinOpToken::And)))
            //             .chain(ast::mk::ts_path(sp, ast::mk::pathx(sp,
            //                 path::mutations(sp),
            //                 vec![Ident::new(mutation.id.into_symbol(), sp)],
            //             )))
            //             .chain(iter::once(ast::mk::ts_token(sp, ast::Spacing::Alone, ast::TokenKind::Comma)))
            //             .collect::<Vec<_>>()
            //     })
            //     // .intersperse(ast::mk::ts_token(sp, ast::Spacing::Alone, ast::TokenKind::Comma))
            //     .collect::<Vec<_>>();
            // let mutations_expr = ast::mk::expr(sp, ast::ExprKind::MacCall(ast::MacCall {
            //     path: ast::mk::path_ident(sp, Ident::new(sym::vec, sp)),
            //     args: P(ast::MacArgs::Delimited(
            //         ast::DelimSpan::from_single(sp),
            //         ast::MacDelimiter::Bracket,
            //         ast::mk::token_stream(mutation_element_tokens),
            //     )),
            //     prior_type_ascription: None,
            // }));

            // &[...]
            let elements = mutant.mutations.iter()
                .map(|mutation| {
                    // &mutations::$mut_id
                    ast::mk::expr_ref(sp, ast::mk::expr_path(ast::mk::pathx(sp,
                        path::mutations(sp),
                        vec![Ident::new(mutation.id.into_symbol(), sp)],
                    )))
                })
                .collect::<Vec<_>>();
            let mutations_expr = ast::mk::expr_slice(sp, elements);

            // SubstMap { ... }
            let subst_map_fields = subst_locs.iter()
                .map(|subst_loc| {
                    // $subst_loc_id: Some(SubstMeta { mutation: &crate::mutest_generated::mutations::$mut_id })
                    let ident = Ident::new(subst_loc.into_subst_loc_id().into_symbol(), sp);
                    let mutation = mutant.mutations.iter().find(|m| m.substs.iter().any(|s| s.location == *subst_loc));
                    let expr = match mutation {
                        Some(mutation) => {
                            // let mutation_path = ast::mk::pathx(sp,
                            //     path::mutations(sp),
                            //     vec![Ident::new(mutation.id.into_symbol(), sp)],
                            // );
                            // ast::mk::expr_call_path(sp, path::Some(sp), vec![
                            //     ast::mk::expr_ref(sp, ast::mk::expr_path(mutation_path)),
                            // ]);

                            let subst_meta_struct_expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::SubstMeta(sp)), vec![
                                ast::mk::expr_struct_field(sp, Ident::new(*sym::mutation, sp), {
                                    let mutation_path = ast::mk::pathx(sp,
                                        path::mutations(sp),
                                        vec![Ident::new(mutation.id.into_symbol(), sp)],
                                    );
                                    ast::mk::expr_ref(sp, ast::mk::expr_path(mutation_path))
                                }),
                            ]);

                            ast::mk::expr_call_path(sp, path::Some(sp), vec![subst_meta_struct_expr])
                        },
                        None => ast::mk::expr_path(path::None(sp)),
                    };
                    ast::mk::expr_struct_field(sp, ident, expr)
                })
                .collect::<Vec<_>>();
            let subst_map_expr = ast::mk::expr_struct(sp, path::SubstMap(sp), subst_map_fields);

            // MutantMeta { ... }
            ast::mk::expr_struct(sp, ast::mk::path_local(path::MutantMeta(sp)), vec![
                ast::mk::expr_struct_field(sp, Ident::new(*sym::mutations, sp), mutations_expr),
                ast::mk::expr_struct_field(sp, Ident::new(*sym::substitutions, sp), subst_map_expr),
            ])
        })
        .collect::<Vec<_>>();

    // const MUTANTS: : &[mutest_runtime::MutantMeta<SubstMap>] = &[ ... ];
    let vis = ast::mk::vis_default(sp);
    let ident = Ident::new(*sym::MUTANTS, sp);
    let mutant_meta_ty = ast::mk::ty_path(None, ast::mk::pathx_args(sp, ast::mk::path_local(path::MutantMeta(sp)), vec![], vec![
        ast::GenericArg::Type(ast::mk::ty_path(None, path::SubstMap(sp))),
    ]));
    let ty = ast::mk::ty_ref(sp, ast::mk::ty_slice(sp, mutant_meta_ty), None);
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
    let expr = ast::mk::expr_call(sp, ast::mk::expr_path(ast::mk::path_local(path::active_mutant_handle_init_empty(sp))), vec![]);
    ast::mk::item_static(sp, vis, mutbl, ident, ty, expr)
}

fn mk_harness_fn(sp: Span) -> P<ast::Item> {
    let test_println = mk_println(sp, "Hello from a generated mutest_harness!");

    // mutest_runtime::mutest_main_static(...);
    let test_runner = ast::mk::expr_path(ast::mk::path_local(path::mutest_main_static(sp)));
    let call_test_main = ast::mk::stmt_expr(ast::mk::expr_call(sp, test_runner, vec![
        ast::mk::expr_ident(sp, Ident::new(*sym::tests, sp)),
        ast::mk::expr_path(path::MUTANTS(sp)),
        ast::mk::expr_ref(sp, ast::mk::expr_path(path::ACTIVE_MUTANT_HANDLE(sp))),
    ]));

    let body = ast::mk::block(sp, vec![test_println, call_test_main]);

    // pub(crate) fn harness(tests: &[&test::TestDescAndFn]) { ... }
    let vis = ast::mk::vis_pub_crate(sp);
    let ident = Ident::new(*sym::harness, sp);
    let inputs = vec![ast::mk::param_ident(sp, Ident::new(*sym::tests, sp), {
        // &[&test::TestDescAndFn]
        let element_ty = ast::mk::ty_ref(sp, ast::mk::ty_path(None, ast::mk::path_local(path::TestDescAndFn(sp))), None);
        ast::mk::ty_ref(sp, ast::mk::ty_slice(sp, element_ty), None)
    })];
    ast::mk::item_fn(sp, vis, ident, None, None, inputs, None, Some(body))
}

struct HarnessGenerator<'op> {
    mutants: &'op Vec<Mutant<'op>>,
    def_site: Span,
}

impl<'op> ast::mut_visit::MutVisitor for HarnessGenerator<'op> {
    fn visit_crate(&mut self, c: &mut ast::Crate) {
        ast::mut_visit::noop_visit_crate(c, self);

        let def = self.def_site;

        let mutations = FxHashSet::from_iter(self.mutants.iter().flat_map(|m| &m.mutations)).into_iter().collect::<Vec<_>>();
        let subst_locs = FxHashSet::from_iter(self.mutants.iter().flat_map(|m| m.iter_substitutions().map(|s| s.location))).into_iter().collect::<Vec<_>>();

        // let mutest_generated = Ident::new(*sym::mutest_generated, def);

        // let mutant = Ident::new(Symbol::intern("Mutant"), def);
        // let active_mutant_handle = Ident::new(Symbol::intern("ACTIVE_MUTANT_HANDLE"), def);
        // let mutants = Ident::new(Symbol::intern("MUTANTS"), def);

        // let mutest_harness = Ident::new(Symbol::intern("harness"), def);
        // let tests = Ident::new(Symbol::intern("tests"), def);

        // #![feature(test)]
        let feature_test_attr = ast::attr::mk_attr_inner(ast::attr::mk_list_item(
            Ident::new(sym::feature, def),
            vec![ast::attr::mk_nested_word_item(Ident::new(sym::test, def))],
        ));
        // #![feature(custom_test_frameworks)]
        let feature_custom_test_frameworks_attr = ast::attr::mk_attr_inner(ast::attr::mk_list_item(
            Ident::new(sym::feature, def),
            vec![ast::attr::mk_nested_word_item(Ident::new(sym::custom_test_frameworks, def))],
        ));
        // #![test_runner(mutest_generated::harness)]
        let test_runner_mutest_harness_attr = ast::attr::mk_attr_inner(ast::attr::mk_list_item(
            Ident::new(sym::test_runner, def),
            vec![ast::NestedMetaItem::MetaItem(ast::MetaItem { span: def, path: ast::mk::path_local(path::harness(def)), kind: ast::MetaItemKind::Word })],
        ));

        c.attrs.push(feature_test_attr);
        c.attrs.push(feature_custom_test_frameworks_attr);
        c.attrs.push(test_runner_mutest_harness_attr);

        // extern crate test;
        let extern_crate_test = ast::mk::item_extern_crate(def, sym::test, None);
        // extern crate mutest_runtime;
        let extern_crate_mutest_runtime = ast::mk::item_extern_crate(def, *sym::mutest_runtime, None);

        // struct Mutant { ... }
        // let mutant_struct_fields = vec![
        //     // pub foo_1: bool,
        //     ast::FieldDef {
        //         id: ast::DUMMY_NODE_ID,
        //         span: def,
        //         attrs: ast::AttrVec::new(),
        //         vis: ast::mk::vis_pub(def),
        //         ident: Some(Ident::new(Symbol::intern("foo_1"), def)),
        //         ty: ast::mk::ty_ident(def, Ident::new(sym::bool, def), None),
        //         is_placeholder: false,
        //     },
        // ];
        // let mutant_struct = P(ast::Item {
        //     id: ast::DUMMY_NODE_ID,
        //     span: def,
        //     attrs: vec![],
        //     vis: ast::mk::vis_pub_crate(def),
        //     ident: mutant,
        //     kind: ast::ItemKind::Struct(ast::VariantData::Struct(mutant_struct_fields, false), ast::Generics::default()),
        //     tokens: None,
        // });

        // const MUTANTS: &[Mutant] = &[...];
        // let mutant_slice_ty = ast::mk::ty(def, ast::TyKind::Slice(ast::mk::ty_ident(def, mutant, None)));
        // let mutant_slice_ref_ty = ast::mk::ty_rptr(def, mutant_slice_ty, None, ast::Mutability::Not);
        // let mutants_const_expr = ast::mk::expr_slice(def, vec![
        //     ast::mk::expr_struct_ident(def, mutant, vec![
        //         ast::mk::expr_struct_field(def, Ident::new(Symbol::intern("foo_1"), def), ast::mk::expr_bool(def, false)),
        //     ]),
        //     ast::mk::expr_struct_ident(def, mutant, vec![
        //         ast::mk::expr_struct_field(def, Ident::new(Symbol::intern("foo_1"), def), ast::mk::expr_bool(def, true)),
        //     ]),
        // ]);
        // let mutants_const = ast::mk::item_const(def,
        //     ast::mk::vis_default(def),
        //     mutants,
        //     mutant_slice_ref_ty,
        //     mutants_const_expr,
        // );

        // pub(crate) static ACTIVE_MUTANT_HANDLE: ActiveMutantHandle<Mutant> = ActiveMutantHandle::empty();
        // let active_mutant_handle_static_ty = ast::mk::ty_path(ast::mk::pathx_args(def,
        //     path::ActiveMutantHandle(def),
        //     vec![],
        //     vec![ast::GenericArg::Type(ast::mk::ty_ident(def, mutant, None))],
        // ), None);
        // let active_mutant_handle_new = ast::mk::expr_path(path::active_mutant_handle_init_empty(def));
        // let active_mutant_handle_static_expr = ast::mk::expr_call(def, active_mutant_handle_new, vec![]);
        // let active_mutant_handle_static = ast::mk::item_static(def,
        //     ast::mk::vis_pub_crate(def),
        //     ast::Mutability::Not,
        //     active_mutant_handle,
        //     active_mutant_handle_static_ty,
        //     active_mutant_handle_static_expr,
        // );

        // let test_println = mk_println(def, "Hello from a generated mutest_harness!");

        // mutest_runtime::mutest_main_static(...);
        // let test_runner = ast::mk::expr_path(path::mutest_main_static(def));
        // let call_test_main = ast::mk::stmt_expr(ast::mk::expr_call(def, test_runner, vec![
        //     ast::mk::expr_ident(def, tests),
        //     // ast::mk::expr_ident(def, mutants),
        //     ast::mk::expr_ident(def, ast::mk::expr_ident(def, Ident::new(*sym::MUTANTS, def))),
        //     // ast::mk::expr_ref(def, ast::mk::expr_path(path::ACTIVE_MUTANT_HANDLE(def))),
        //     ast::mk::expr_ref(def, ast::mk::expr_ident(def, Ident::new(*sym::ACTIVE_MUTANT_HANDLE, def))),
        // ]));

        // let harness_body = ast::mk::block(def, vec![test_println, call_test_main]);

        // &[&test::TestDescAndFn]
        // let test_descriptor_ty = ast::mk::ty_path(path::TestDescAndFn(def), None);
        // let test_descriptor_ref_ty = ast::mk::ty_ref(def, test_descriptor_ty, None);
        // let test_descriptor_ref_slice_ty = ast::mk::ty_slice(def, test_descriptor_ref_ty);
        // let test_descriptor_ref_slice_ref_ty = ast::mk::ty_ref(def, test_descriptor_ref_slice_ty, None);

        // fn harness(tests: &[&test::TestDescAndFn]) { ... }
        // let harness = P(ast::Item {
        //     id: ast::DUMMY_NODE_ID,
        //     span: def,
        //     attrs: vec![],
        //     vis: ast::mk::vis_pub_crate(def),
        //     ident: mutest_harness,
        //     kind: ast::ItemKind::Fn(Box::new(ast::Fn {
        //         defaultness: ast::Defaultness::Final,
        //         generics: Default::default(),
        //         sig: ast::FnSig {
        //             span: def,
        //             header: ast::FnHeader::default(),
        //             decl: ecx.fn_decl(
        //                 vec![
        //                     ast::Param {
        //                         id: ast::DUMMY_NODE_ID,
        //                         span: def,
        //                         attrs: ast::AttrVec::new(),
        //                         pat: ecx.pat_ident(def, tests),
        //                         ty: test_descriptor_ref_slice_ref_ty,
        //                         is_placeholder: false,
        //                     },
        //                 ],
        //                 ast::FnRetTy::Default(def),
        //             ),
        //         },
        //         body: Some(harness_body),
        //     })),
        //     tokens: None,
        // });

        // pub(crate) mod mutest_generated { ... }
        let mutest_generated_mod = ast::mk::item_mod(def,
            ast::mk::vis_pub_crate(def),
            Ident::new(*sym::mutest_generated, def),
            vec![
                extern_crate_test,
                extern_crate_mutest_runtime,
                mk_subst_map_struct(def, &subst_locs),
                mk_mutations_mod(def, &mutations),
                mk_mutants_slice_const(def, self.mutants, &subst_locs),
                mk_active_mutant_handle_static(def),
                mk_harness_fn(def),
            ],
        );

        c.items.push(mutest_generated_mod);
    }
}

pub fn generate_harness(ecx: &mut ExtCtxt<'_>, mutants: &Vec<Mutant>, krate: &mut ast::Crate) {
    let expn_id = ecx.resolver.expansion_for_ast_pass(
        DUMMY_SP,
        AstPass::TestHarness,
        &[sym::test, sym::rustc_attrs],
        None,
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut generator = HarnessGenerator { mutants, def_site };
    generator.visit_crate(krate);
}
