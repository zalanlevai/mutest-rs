use rustc_hir as hir;
use rustc_middle::ty::{TyCtxt, self};

struct HirVisitor<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}

impl<'tcx> hir::intravisit::Visitor<'tcx> for HirVisitor<'tcx> {
    // type Map = Map<'tcx>;
    //
    // fn nested_visit_map(&mut self) -> hir::intravisit::NestedVisitorMap<Self::Map> {
    //     hir::intravisit::NestedVisitorMap::OnlyBodies(self.tcx.hir())
    // }

    fn visit_body(&mut self, body: &'tcx hir::Body<'tcx>) {
        let typeck_results = self.tcx.typeck_body(body.id());

        fn inspect_ty<'sess, 'tcx>(tcx: TyCtxt<'tcx>, typeck_results: &'tcx ty::TypeckResults, expr: &hir::Expr) {
            let ty = typeck_results.expr_ty(expr);

            let snippet_with_location = match tcx.sess.source_map().span_to_snippet(expr.span) {
                Ok(snippet) if snippet.contains("\n") => format!("{:#?} ```\n{}\n```", expr.span, snippet),
                Ok(snippet) => format!("{:#?} `{}`", expr.span, snippet),
                _ => format!("{:#?}", expr.span),
            };

            let ty_relation_to_bool = match ty == tcx.types.bool {
                true => "===",
                false => "!==",
            };

            println!("{} -> {} {} bool", snippet_with_location, ty, ty_relation_to_bool);

            let mut err = tcx.sess.struct_span_warn(expr.span, &format!("[mutest] type here is {}", ty));
            err.emit();

            match expr.kind {
                hir::ExprKind::If(condition, if_body, else_body) => {
                    inspect_ty(tcx, typeck_results, condition);
                    inspect_ty(tcx, typeck_results, if_body);

                    if let Some(expr) = else_body {
                        inspect_ty(tcx, typeck_results, expr);
                    }
                },
                _ => {},
            }
        }

        match body.value.kind {
            hir::ExprKind::Block(block, _) => {
                for stmt in block.stmts {
                    match stmt.kind {
                        hir::StmtKind::Local(local) => {
                            if let Some(init_expr) = local.init {
                                inspect_ty(self.tcx, typeck_results, init_expr);
                            }
                        },
                        hir::StmtKind::Expr(expr) => {
                            inspect_ty(self.tcx, typeck_results, expr);
                        },
                        hir::StmtKind::Semi(expr) => {
                            inspect_ty(self.tcx, typeck_results, expr);
                        },
                        _ => {},
                    }
                }

                if let Some(expr) = block.expr {
                    inspect_ty(self.tcx, typeck_results, expr);
                }
            },
            _ => {},
        }

        hir::intravisit::walk_body(self, body);
    }
}
