//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

use std::cmp::Ordering;

#[derive(PartialEq, Eq)]
struct ImplsOrd;

fn partial_cmp_impl() {}
fn lt_impl() {}
fn le_impl() {}
fn gt_impl() {}
fn ge_impl() {}

impl PartialOrd for ImplsOrd {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        partial_cmp_impl();
        Some(Ordering::Equal)
    }

    fn lt(&self, other: &Self) -> bool {
        lt_impl();
        matches!(self.partial_cmp(other), Some(Ordering::Less))
    }

    fn le(&self, other: &Self) -> bool {
        le_impl();
        matches!(self.partial_cmp(other), Some(Ordering::Less | Ordering::Equal))
    }

    fn gt(&self, other: &Self) -> bool {
        gt_impl();
        matches!(self.partial_cmp(other), Some(Ordering::Greater))
    }

    fn ge(&self, other: &Self) -> bool {
        ge_impl();
        matches!(self.partial_cmp(other), Some(Ordering::Greater | Ordering::Equal))
    }
}

#[test]
fn test_implicit_lt_call() {
    let _ = ImplsOrd < ImplsOrd;
}

#[test]
fn test_implicit_le_call() {
    let _ = ImplsOrd <= ImplsOrd;
}

#[test]
fn test_implicit_gt_call() {
    let _ = ImplsOrd > ImplsOrd;
}

#[test]
fn test_implicit_ge_call() {
    let _ = ImplsOrd >= ImplsOrd;
}
