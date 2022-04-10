use rustc_ast as ast;
use rustc_ast::ptr::P;
use rustc_ast::visit::Visitor;
use rustc_session::Session;
use rustc_span::sym;
use rustc_span::symbol::Ident;

#[derive(Debug)]
pub struct Test {
    pub path: Vec<Ident>,
    pub descriptor: P<ast::Item>,
    pub item: P<ast::Item>,
}

fn is_test_case(sess: &Session, item: &ast::Item) -> bool {
    sess.contains_name(&item.attrs, sym::rustc_test_marker)
}

fn extract_expanded_tests(sess: &Session, path: &Vec<Ident>, items: &Vec<P<ast::Item>>) -> Vec<Test> {
    let mut tests = vec![];

    let mut item_iterator = items.iter();
    while let Some(item) = item_iterator.next() {
        if !is_test_case(sess, item) {
            continue;
        }

        let test_case = item;
        let test_item = item_iterator.next().expect("test case not followed by the test item");

        tests.push(Test {
            path: path.to_owned(),
            descriptor: test_case.to_owned(),
            item: test_item.to_owned(),
        });
    }

    tests
}

struct TestCollector<'a> {
    sess: &'a Session,
    current_path: Vec<Ident>,
    tests: Vec<Test>,
}

impl<'ast> Visitor<'ast> for TestCollector<'_> {
    fn visit_crate(&mut self, c: &'ast ast::Crate) {
        let mut tests = extract_expanded_tests(self.sess, &self.current_path, &c.items);
        self.tests.append(&mut tests);

        ast::visit::walk_crate(self, c);
    }

    fn visit_item(&mut self, i: &'ast ast::Item) {
        if let ast::ItemKind::Mod(.., ast::ModKind::Loaded(ref items, ..)) = i.kind {
            self.current_path.push(i.ident);

            let mut tests = extract_expanded_tests(self.sess, &self.current_path, &items);
            self.tests.append(&mut tests);

            ast::visit::walk_item(self, i);

            self.current_path.pop();
        }
    }
}

pub fn collect_tests(sess: &Session, krate: &ast::Crate) -> Vec<Test> {
    let mut collector = TestCollector { sess, current_path: vec![], tests: vec![] };
    collector.visit_crate(krate);

    collector.tests
}
