//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

use std::ops::IndexMut;

macro m() {
    trait Choose: IndexMut<usize> {
        fn choose(&self) -> Option<&Self::Output>;
    }

    trait ChooseGeneric<T>: IndexMut<T> {
        fn choose(&self) -> Option<&Self::Output>;
    }
}

m!();
