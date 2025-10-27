use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;

use rustc_middle::ty::TyCtxt;
use rustc_session::Session;
use thin_vec::{ThinVec, thin_vec};

use crate::analysis::ast_lowering;
use crate::analysis::hir;
use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::symbols::{DUMMY_SP, FileNameDisplayPreference, Ident, Symbol, path, sym};

pub struct Test {
    pub path: Vec<Ident>,
    pub descriptor: P<ast::Item>,
    pub item: P<ast::Item>,
    pub def_id: hir::LocalDefId,
    pub ignore: bool,
}

impl Test {
    pub fn path_str(&self) -> String {
        self.path.iter()
            .map(|segment| segment.name.as_str())
            .intersperse("::")
            .collect::<String>()
    }
}

impl Eq for Test {}
impl PartialEq for Test {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Hash for Test {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

impl Debug for Test {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Test(")?;
        for (i, segment) in self.path.iter().enumerate() {
            if i >= 1 { write!(f, "::")?; }
            write!(f, "{}", segment.name.as_str())?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

fn is_test_case(item: &ast::Item) -> bool {
    item.attrs.iter().any(|attr| attr.has_name(sym::rustc_test_marker))
}

fn extract_expanded_tests(def_res: &ast_lowering::DefResolutions, path: &[Ident], items: &[P<ast::Item>]) -> Vec<Test> {
    let mut tests = vec![];

    let mut item_iterator = items.iter();
    while let Some(item) = item_iterator.next() {
        if !is_test_case(item) {
            continue;
        }

        let test_case = item;

        let test_item = item_iterator.next().expect("test case not followed by the test item");
        let Some(def_id) = def_res.node_id_to_def_id.get(&test_item.id).copied() else { unreachable!(); };

        let Some(ident) = test_case.kind.ident() else { panic!("encountered test case without ident"); };

        let ignore = test_item.attrs.iter().any(|attr| attr.has_name(sym::ignore));

        tests.push(Test {
            path: path.iter().copied().chain(iter::once(ident)).collect(),
            descriptor: test_case.to_owned(),
            item: test_item.to_owned(),
            def_id,
            ignore,
        });
    }

    tests
}

struct TestCollector<'op> {
    current_path: Vec<Ident>,
    tests: Vec<Test>,
    def_res: &'op ast_lowering::DefResolutions,
}

impl<'ast, 'op> ast::visit::Visitor<'ast> for TestCollector<'op> {
    fn visit_crate(&mut self, krate: &'ast ast::Crate) {
        let mut tests = extract_expanded_tests(self.def_res, &self.current_path, &krate.items);
        self.tests.append(&mut tests);

        ast::visit::walk_crate(self, krate);
    }

    fn visit_item(&mut self, item: &'ast ast::Item) {
        if let ast::ItemKind::Mod(_, _, ast::ModKind::Loaded(ref items, _, _, _)) = item.kind {
            let ident = item.kind.ident();

            if let Some(ident) = ident { self.current_path.push(ident); }

            let mut tests = extract_expanded_tests(self.def_res, &self.current_path, &items);
            self.tests.append(&mut tests);

            ast::visit::walk_item(self, item);

            if let Some(_ident) = ident { self.current_path.pop(); }
        }
    }
}

pub fn collect_tests(krate: &ast::Crate, def_res: &ast_lowering::DefResolutions) -> Vec<Test> {
    let mut collector = TestCollector { current_path: vec![], tests: vec![], def_res };
    collector.visit_crate(krate);

    collector.tests
}

fn extract_and_mark_expanded_embedded_tests(sess: &Session, def_res: &ast_lowering::DefResolutions, tests: &mut Vec<Test>, path: &[Ident], items: &mut ThinVec<P<ast::Item>>) {
    let g = &sess.psess.attr_id_generator;
    let sp = DUMMY_SP;

    let mut any_tests_in_scope = false;

    let mut i = 0;
    while i < items.len() {
        let item = &items[i];

        if !item.attrs.iter().any(|attr| attr.has_name(sym::link_section) && attr.value_str() == Some(Symbol::intern(".embedded_test.tests"))) {
            i += 1;
            continue;
        }

        let test_symbol = item;
        let item_export_name_value_str = test_symbol.attrs.iter().find_map(|attr| attr.has_name(sym::export_name).then_some(attr.value_str()).flatten()).expect("test symbol missing metadata");

        let test_entry_point = items.get(i - 1).expect("test symbol not preceeded by test entry point");
        let Some(test_entry_point_ident) = test_entry_point.kind.ident() else { panic!("encountered test entry point without ident"); };
        if !test_entry_point_ident.name.as_str().ends_with("_entrypoint") { panic!("test entry point has unexpected ident"); };

        let test_item = items.get(i - 2).expect("test entry point not preceeded by test item");
        let Some(def_id) = def_res.node_id_to_def_id.get(&test_item.id).copied() else { unreachable!(); };
        let Some(ident) = test_item.kind.ident() else { panic!("encountered test item without ident"); };

        let test_item = test_item.to_owned();
        let test_path = path.iter().copied().chain(iter::once(ident)).collect::<Vec<_>>();

        let test_path_str = test_path.iter()
            .map(|segment| segment.name.as_str())
            .intersperse("::")
            .collect::<String>();

        let ignore = match item_export_name_value_str.as_str().split_once("\"ignored\":") {
            Some((_, s)) if let Some((v_str, _)) = s.split_once([',', '}']) => match v_str {
                "true" => true,
                "false" => false,
                _ => panic!("encountered unexpected value for embedded-test `#[export_name]` test metadata key `ignored`: `{v_str}`"),
            }
            _ => false,
        };
        let should_panic = match item_export_name_value_str.as_str().split_once("\"should_panic\":") {
            Some((_, s)) if let Some((v_str, _)) = s.split_once([',', '}']) => match v_str {
                "true" => true,
                "false" => false,
                _ => panic!("encountered unexpected value for embedded-test `#[export_name]` test metadata key `should_panic`: `{v_str}`"),
            }
            _ => false,
        };

        let (source_file, lo_line, lo_col, hi_line, hi_col) = sess.source_map().span_to_location_info(ident.span);
        let file_name = match source_file {
            Some(source_file) => source_file.name.display(FileNameDisplayPreference::Remapped).to_string(),
            None => "no-location".to_owned(),
        };

        let test_descr_expr = ast::mk::expr_struct(sp, ast::mk::path_local(path::EmbeddedTestDescAndFn(sp)), thin_vec![
            ast::mk::expr_struct_field(sp, Ident::new(sym::name, sp), {
                ast::mk::expr_str(sp, &test_path_str)
            }),

            ast::mk::expr_struct_field(sp, Ident::new(sym::source_file, sp), {
                ast::mk::expr_str(sp, &file_name)
            }),
            ast::mk::expr_struct_field(sp, Ident::new(sym::start_line, sp), {
                ast::mk::expr_usize(sp, lo_line)
            }),
            ast::mk::expr_struct_field(sp, Ident::new(sym::start_col, sp), {
                ast::mk::expr_usize(sp, lo_col)
            }),
            ast::mk::expr_struct_field(sp, Ident::new(sym::end_line, sp), {
                ast::mk::expr_usize(sp, hi_line)
            }),
            ast::mk::expr_struct_field(sp, Ident::new(sym::end_col, sp), {
                ast::mk::expr_usize(sp, hi_col)
            }),

            ast::mk::expr_struct_field(sp, Ident::new(sym::ignore, sp), {
                ast::mk::expr_bool(sp, ignore)
            }),
            ast::mk::expr_struct_field(sp, Ident::new(sym::should_panic, sp), {
                ast::mk::expr_bool(sp, should_panic)
            }),

            ast::mk::expr_struct_field(sp, Ident::new(sym::test_fn, sp), {
                ast::mk::expr_ident(sp, test_entry_point_ident)
            }),
        ]);

        // #[rustc_test_marker = "..."]
        let rustc_test_marker_attr = ast::mk::attr_outer(g, sp,
            Ident::new(sym::rustc_test_marker, sp),
            ast::AttrArgs::Eq { eq_span: sp, expr: ast::mk::expr_str(sp, &test_path_str) },
        );

        let vis = ast::mk::vis_default(sp);
        let ident = Ident::new(Symbol::intern(&format!("__mutest_generated_{}_descr", ident.name)), sp);
        let ty = ast::mk::ty_path(None, ast::mk::path_local(path::EmbeddedTestDescAndFn(sp)));
        let mut descriptor = ast::mk::item_const(sp, vis, ident, ty, test_descr_expr);
        descriptor.attrs.push(rustc_test_marker_attr);
        // NOTE: Replace the symbol item with our test descriptor.
        items.splice(i..=i, [descriptor.clone()]);

        any_tests_in_scope = true;

        tests.push(Test {
            path: test_path,
            descriptor,
            item: test_item,
            def_id,
            ignore,
        });

        // NOTE: Skip past the inserted item.
        i += 1;
    }

    if any_tests_in_scope {
        // extern crate mutest_runtime;
        if !items.iter().any(|item| ast::inspect::is_extern_crate_decl(item, sym::mutest_runtime)) {
            items.push(ast::mk::item_extern_crate(sp, sym::mutest_runtime, None));
        }
    }
}

struct EmbeddedTestCollector<'tcx, 'op> {
    sess: &'tcx Session,
    def_res: &'op ast_lowering::DefResolutions,
    current_path: Vec<Ident>,
    tests: Vec<Test>,
}

impl<'tcx, 'op> ast::mut_visit::MutVisitor for EmbeddedTestCollector<'tcx, 'op> {
    fn visit_crate(&mut self, krate: &mut ast::Crate) {
        extract_and_mark_expanded_embedded_tests(self.sess, self.def_res, &mut self.tests, &self.current_path, &mut krate.items);
        ast::mut_visit::walk_crate(self, krate);
    }

    fn visit_item(&mut self, item: &mut ast::Item) {
        let ident = item.kind.ident();

        if let ast::ItemKind::Mod(_, _, ast::ModKind::Loaded(ref mut items, _, _, _)) = item.kind {
            if let Some(ident) = ident { self.current_path.push(ident); }

            extract_and_mark_expanded_embedded_tests(self.sess, self.def_res, &mut self.tests, &self.current_path, items);
            ast::mut_visit::walk_item(self, item);

            if let Some(_ident) = ident { self.current_path.pop(); }
        }
    }
}

pub fn collect_and_mark_embedded_tests(sess: &Session, krate: &mut ast::Crate, def_res: &ast_lowering::DefResolutions) -> Vec<Test> {
    use crate::codegen::ast::mut_visit::MutVisitor;

    let mut collector = EmbeddedTestCollector { sess, def_res, current_path: vec![], tests: vec![] };
    collector.visit_crate(krate);

    collector.tests
}

pub fn is_marked_or_in_cfg_test<'tcx>(tcx: TyCtxt<'tcx>, id: hir::HirId) -> bool {
    iter::once(id).chain(tcx.hir_parent_id_iter(id)).any(|parent_id| {
        tcx.hir_attrs(parent_id).iter().any(|attr| {
            // NOTE: `cfg(true)` attributes now leave `cfg_trace(true)` attributes behind after expansion.
            //       This is how we can detect the original cfgs in the source code.
            //       See https://github.com/rust-lang/rust/pull/138844.
            hir::attr::is_list_attr_with_ident(attr, None, sym::cfg_trace, sym::test)
        })
    })
}
