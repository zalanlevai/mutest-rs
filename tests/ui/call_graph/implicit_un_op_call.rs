//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

use std::ops::{Deref, DerefMut, Neg, Not};

struct ImplsDeref;

fn deref_impl() {}
impl Deref for ImplsDeref {
    type Target = Self;

    fn deref(&self) -> &Self::Target {
        deref_impl();
        self
    }
}

#[test]
fn test_implicit_deref_call() {
    let _ = *ImplsDeref;
}

fn deref_mut_impl() {}
impl DerefMut for ImplsDeref {
    fn deref_mut(&mut self) -> &mut Self::Target {
        deref_mut_impl();
        self
    }
}

#[test]
fn test_implicit_deref_mut_call() {
    *ImplsDeref = ImplsDeref;
}

struct ImplsNeg;

fn neg_impl() {}
impl Neg for ImplsNeg {
    type Output = Self;

    fn neg(self) -> Self::Output {
        neg_impl();
        self
    }
}

#[test]
fn test_implicit_neg_call() {
    let _ = -ImplsNeg;
}

struct ImplsNot;

fn not_impl() {}
impl Not for ImplsNot {
    type Output = Self;

    fn not(self) -> Self::Output {
        not_impl();
        self
    }
}

#[test]
fn test_implicit_not_call() {
    let _ = !ImplsNot;
}
