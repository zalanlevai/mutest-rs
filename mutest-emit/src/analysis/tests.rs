use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;

use crate::codegen::ast;
use crate::codegen::ast::P;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::symbols::{Ident, sym};

pub struct Test {
    pub path: Vec<Ident>,
    pub descriptor: P<ast::Item>,
    pub item: P<ast::Item>,
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

fn extract_expanded_tests(path: &Vec<Ident>, items: &Vec<P<ast::Item>>) -> Vec<Test> {
    let mut tests = vec![];

    let mut item_iterator = items.iter();
    while let Some(item) = item_iterator.next() {
        if !is_test_case(item) {
            continue;
        }

        let test_case = item;
        let test_item = item_iterator.next().expect("test case not followed by the test item");

        tests.push(Test {
            path: path.iter().copied().chain(iter::once(test_case.ident)).collect(),
            descriptor: test_case.to_owned(),
            item: test_item.to_owned(),
        });
    }

    tests
}

struct TestCollector {
    current_path: Vec<Ident>,
    tests: Vec<Test>,
}

impl<'ast> ast::visit::Visitor<'ast> for TestCollector {
    fn visit_crate(&mut self, c: &'ast ast::Crate) {
        let mut tests = extract_expanded_tests(&self.current_path, &c.items);
        self.tests.append(&mut tests);

        ast::visit::walk_crate(self, c);
    }

    fn visit_item(&mut self, i: &'ast ast::Item) {
        if let ast::ItemKind::Mod(.., ast::ModKind::Loaded(ref items, ..)) = i.kind {
            self.current_path.push(i.ident);

            let mut tests = extract_expanded_tests(&self.current_path, &items);
            self.tests.append(&mut tests);

            ast::visit::walk_item(self, i);

            self.current_path.pop();
        }
    }
}

pub fn collect_tests(krate: &ast::Crate) -> Vec<Test> {
    let mut collector = TestCollector { current_path: vec![], tests: vec![] };
    collector.visit_crate(krate);

    collector.tests
}
