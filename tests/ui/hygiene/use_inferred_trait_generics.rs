//@ build
//@ stderr: empty

//! This test shows that when qualifying paths to trait assoc items,
//! it is sometimes necessary to use the inferred trait generics from the type system.
//!
//! This test is based on clap's `<ValueParser as From<T>>` impls,
//! and is triggered by the incorrect assumption that
//! a `Self::from` path can only refer to `Self: From<T>` item constraints that are in scope.
//! This is not the case, as such paths may also refer to impls with different generic args that
//! can be inferred by the type system.

#![feature(decl_macro)]

#![allow(unused)]

use std::ops::{Bound, Range, RangeBounds};

macro m() {
    // TEST: Use inferred generics for qualifying the trait of assoc item paths.
    trait AnyValueParser: 'static {}
    pub struct ValueParser(Box<dyn AnyValueParser>);
    impl<T: AnyValueParser> From<T> for ValueParser {
        fn from(value: T) -> Self {
            Self(Box::new(value))
        }
    }
    pub struct RangedI64ValueParser {
        bounds: (Bound<i64>, Bound<i64>),
    }
    impl AnyValueParser for RangedI64ValueParser {}
    impl From<Range<i64>> for ValueParser {
        fn from(value: Range<i64>) -> Self {
            let bounds = (value.start_bound().cloned(), value.end_bound().cloned());
            let inner = RangedI64ValueParser { bounds };
            Self::from(inner)
        }
    }

    // TEST: Ensure that only trait generics are written out, not generic args for the inner assoc items.
    trait TraitWithGenericFn<T> {
        fn foo<U>(value: T) -> Self;
    }
    impl<T: AnyValueParser> TraitWithGenericFn<T> for ValueParser {
        fn foo<U>(value: T) -> Self {
            Self(Box::new(value))
        }
    }
    impl TraitWithGenericFn<Range<i64>> for ValueParser {
        fn foo<U>(value: Range<i64>) -> Self {
            let bounds = (value.start_bound().cloned(), value.end_bound().cloned());
            let inner = RangedI64ValueParser { bounds };
            Self::foo::<()>(inner)
        }
    }
}

m!();
