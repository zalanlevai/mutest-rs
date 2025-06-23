//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

use std::ops::{Index, IndexMut};

struct ImplsIndex;

fn index_impl() {}
impl Index<usize> for ImplsIndex {
    type Output = Self;

    fn index(&self, _index: usize) -> &Self::Output {
        index_impl();
        self
    }
}

fn index_mut_impl() {}
impl IndexMut<usize> for ImplsIndex {
    fn index_mut(&mut self, _index: usize) -> &mut Self::Output {
        index_mut_impl();
        self
    }
}

#[test]
fn test_implicit_index_call() {
    let _ = ImplsIndex[0];
}

#[test]
fn test_implicit_index_mut_call() {
    ImplsIndex[0] = ImplsIndex;
}
