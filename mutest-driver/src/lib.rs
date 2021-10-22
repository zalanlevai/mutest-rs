#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_typeck;

use rustc_ast as ast;
use rustc_errors::registry::Registry;
use rustc_hir as hir;
use rustc_hir::intravisit::Visitor;
use rustc_infer::infer::TyCtxtInferExt;
use rustc_infer::traits::ObligationCause;
use rustc_interface::{Config, run_compiler};
use rustc_middle::hir::map::Map;
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_session::DiagnosticOutput;
use rustc_session::config::{Input, Options};
use rustc_session::search_paths::SearchPath;
use rustc_span::source_map::{FileName, RealFileLoader};
use rustc_typeck::check::{FnCtxt, Inherited};
use rustc_typeck::hir_ty_to_ty;

// struct AstVisitor<'tcx> {
//     pub tcx: TyCtxt<'tcx>,
// }
//
// impl<'tcx, 'ast> ast::visit::Visitor<'ast> for AstVisitor<'tcx> {
//     // fn visit_block(&mut self, block: &'ast ast::Block) {
//     // }
//
//     fn visit_stmt(&mut self, stmt: &'ast ast::Stmt) {
//         let hir_map = self.tcx.hir();
//
//         match &stmt.kind {
//             ast::StmtKind::Local(local) => {
//                 match &local.kind {
//                     ast::LocalKind::Init(expr) | ast::LocalKind::InitElse(expr, _) => {
//                         let def_id = hir_map.find(expr.id);
//
//                         let ty = self.tcx.type_of(expr.id);
//                     },
//                     _ => {},
//                 }
//
//                 ast::visit::walk_local(self, local);
//             },
//             _ => {},
//         }
//
//         ast::visit::walk_stmt(self, stmt);
//     }
// }

struct HirVisitor<'tcx> {
    pub tcx: TyCtxt<'tcx>
}

impl<'tcx> hir::intravisit::Visitor<'tcx> for HirVisitor<'tcx> {
    type Map = Map<'tcx>;

    fn nested_visit_map(&mut self) -> hir::intravisit::NestedVisitorMap<Self::Map> {
        hir::intravisit::NestedVisitorMap::OnlyBodies(self.tcx.hir())
    }

    fn visit_body(&mut self, body: &'tcx hir::Body<'tcx>) {
        let parent_id = self.tcx.hir().get_parent_item(body.id().hir_id);
        let parent_local_def_id = self.tcx.hir().local_def_id(parent_id);
        // let (thir, expr_id) = self.tcx.thir_body(WithOptConstParam::unknown(local_def_id));

        let typeck_results = self.tcx.typeck_body(body.id());

        match body.value.kind {
            hir::ExprKind::Block(block, _) => {
                for stmt in block.stmts {
                    match stmt.kind {
                        hir::StmtKind::Local(local) => {
                            if let Some(init_expr) = local.init {
                                let ty = typeck_results.expr_ty(init_expr);
                                println!("{:#?} {} -> {:#?}", init_expr.span, self.tcx.hir().node_to_string(init_expr.hir_id), ty);
                            }
                        },
                        _ => {},
                    }
                }
            },
            _ => {},
        }

        // self.tcx.infer_ctxt().enter(|infcx| {
        //     // let fncx = FnCtxt::new(&Inherited::new(infcx, local_def_id), ty::ParamEnv::empty(), body.id().hir_id);

        //     Inherited::build(self.tcx, parent_local_def_id).enter(|inh| {
        //         let param_env = self.tcx.param_env(parent_local_def_id);
        //         let fncx = FnCtxt::new(&inh, param_env, body.id().hir_id);

        //         match body.value.kind {
        //             hir::ExprKind::Block(block, _) => {
        //                 for stmt in block.stmts {
        //                     match stmt.kind {
        //                         hir::StmtKind::Local(local) => {
        //                             if let Some(init_expr) = local.init {
        //                                 fncx.check_expr_has_type_or_error(init_expr, self.tcx.types.bool, |_| {});
        //                                 println!("{:#?} {} -> {:#?}", init_expr.span, self.tcx.hir().node_to_string(init_expr.hir_id), fncx.node_ty(init_expr.hir_id));
        //                             }
        //                         },
        //                         _ => {},
        //                     }
        //                 }
        //             },
        //             _ => {},
        //         }
        //     });

        //     // infcx.at(&ObligationCause::dummy(), ty::ParamEnv::empty()).eq(self.tcx.types.bool, )
        // });

        hir::intravisit::walk_body(self, body);
    }

    // fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt<'tcx>) {
    //     match stmt.kind {
    //         hir::StmtKind::Local(local) => {
    //             if let Some(init_expr) = local.init {
    //                 let local_stmt_local_def_id = self.tcx.hir().local_def_id(local.hir_id);
    //                 // let init_expr_local_def_id = self.tcx.hir().local_def_id(init_expr.hir_id);
    //
    //                 let ty = self.tcx.type_of(local_stmt_local_def_id);
    //                 // let ty = self.tcx.type_of(init_expr_local_def_id);
    //
    //                 println!("{:#?}", ty);
    //             }
    //
    //             hir::intravisit::walk_local(self, local);
    //         },
    //         _ => {},
    //     }
    //
    //     hir::intravisit::walk_stmt(self, stmt);
    // }

    // fn visit_stmt(&mut self, stmt: &'tcx hir::Stmt<'tcx>) {
    //     match stmt.kind {
    //         hir::StmtKind::Local(local) => {
    //             if let Some(init_expr) = local.init {
    //                 // let init_expr_ty =
    //
    //                 self.tcx.infer_ctxt().enter(|infcx| {
    //                     let fncx = FnCtxt::new(&Inherited::new(infcx, _), ty::ParamEnv::empty());
    //
    //                     fncx.check_expr();
    //
    //                     infcx.at(&ObligationCause::dummy(), ty::ParamEnv::empty()).eq(self.tcx.types.bool, )
    //                 });
    //
    //                 println!("{:#?}", ty);
    //             }
    //
    //             hir::intravisit::walk_local(self, local);
    //         },
    //         _ => {},
    //     }
    //
    //     hir::intravisit::walk_stmt(self, stmt);
    // }
}

pub fn main() {
    println!("Hello from mutest_driver::main");

    let config = Config {
        opts: Options {
            search_paths: vec![
                SearchPath::from_sysroot_and_triple(std::path::Path::new("/Users/zalanlevai/.rustup/toolchains/nightly-2021-10-20-aarch64-apple-darwin"), "aarch64-apple-darwin"),
            ],
            ..Default::default()
        },

        crate_cfg: Default::default(),

        input: Input::Str {
            name: FileName::Custom("foo.rs <mutated>".to_owned()),
            input: r#"
                fn main() {
                    let truth = 1 < 2;
                    let n = 0;
                    let txt = "hello";
                    println!("{}", truth);
                }
            "#.to_owned(),
            // input: r#"
            //     #![feature(no_core)]
            //     #![no_core]
            //     #![no_std]

            //     fn main() {
            //         let truth = 1 < 2;
            //     }
            // "#.to_owned(),
        },
        input_path: None,
        output_dir: None,
        output_file: None,
        // file_loader: None,
        file_loader: Some(Box::new(RealFileLoader)),
        diagnostic_output: DiagnosticOutput::Default,

        stderr: None,

        lint_caps: Default::default(),

        parse_sess_created: None,
        register_lints: None,
        override_queries: None,
        make_codegen_backend: None,

        registry: Registry::new(Default::default()),
    };

    println!("{:#?}", config.opts.search_paths);

    run_compiler(config, |compiler| {
        let session = compiler.session();

        compiler.enter(|queries| -> rustc_interface::interface::Result<()> {
            let crate_ast = queries.parse()?;
            println!("{:#?}", crate_ast.peek());

            queries.global_ctxt()?.peek_mut().enter(|tcx| {
                // let analysis = tcx.analysis(());

                println!("{:#?}", tcx.hir().krate());

                tcx.hir().visit_all_item_likes(&mut HirVisitor { tcx }.as_deep_visitor());

                // for item in hir_map.items() {
                //     match item.kind {
                //         hir::ItemKind::Fn(signature, generics, body_id) => {
                //             let body = hir_map.body(body_id);
                //             // let body_expr = body.value;
                //
                //             hir_map.as_local_node_id();
                //         },
                //         _ => {},
                //     };
                // }
            });

            Ok(())
        }).unwrap();
    });
}
