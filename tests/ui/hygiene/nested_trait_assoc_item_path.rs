//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

use std::marker::PhantomData;

macro m() {
    trait HasCombination<I>: Sized {
        type Combination: From<I> + Iterator<Item = Self>;
    }

    struct TupleCombinations<I, T>
    where
        I: Iterator,
        T: HasCombination<I>,
    {
        iter: T::Combination,
        _phantom: PhantomData<I>,
    }

    fn tuple_combinations<T, I>(iter: I) -> TupleCombinations<I, T>
    where
        I: Iterator + Clone,
        I::Item: Clone,
        T: HasCombination<I>,
    {
        TupleCombinations {
            iter: T::Combination::from(iter),
            _phantom: PhantomData,
        }
    }
}

m!();
