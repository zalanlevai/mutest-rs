use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;

use rustc_middle::ty::TyCtxt;

use crate::analysis::ast_lowering;
use crate::analysis::hir;
use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::symbols::{Ident, sym};

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

        let ignore = test_item.attrs.iter().any(|attr| attr.has_name(sym::ignore));

        tests.push(Test {
            path: path.iter().copied().chain(iter::once(test_case.ident)).collect(),
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
    fn visit_crate(&mut self, c: &'ast ast::Crate) {
        let mut tests = extract_expanded_tests(self.def_res, &self.current_path, &c.items);
        self.tests.append(&mut tests);

        ast::visit::walk_crate(self, c);
    }

    fn visit_item(&mut self, i: &'ast ast::Item) {
        if let ast::ItemKind::Mod(.., ast::ModKind::Loaded(ref items, ..)) = i.kind {
            self.current_path.push(i.ident);

            let mut tests = extract_expanded_tests(self.def_res, &self.current_path, &items);
            self.tests.append(&mut tests);

            ast::visit::walk_item(self, i);

            self.current_path.pop();
        }
    }
}

pub fn collect_tests(krate: &ast::Crate, def_res: &ast_lowering::DefResolutions) -> Vec<Test> {
    let mut collector = TestCollector { current_path: vec![], tests: vec![], def_res };
    collector.visit_crate(krate);

    collector.tests
}

pub fn is_marked_or_in_cfg_test<'tcx>(tcx: TyCtxt<'tcx>, id: hir::HirId) -> bool {
    iter::once(id).chain(tcx.hir().parent_id_iter(id)).any(|parent_id| {
        tcx.hir().attrs(parent_id).iter().any(|attr| {
            ast::inspect::is_list_attr_with_ident(attr, None, sym::cfg, sym::test)
        })
    })
}
