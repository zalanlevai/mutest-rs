//@ build
//@ stderr

//! This test shows that implicit generic args are not always representable,
//! because there is no requirement for the type to be visible
//! (unless the type is visible from outside of the crate).
//! In such cases, the best we can do to name the type is to use
//! an inference type in place of the unrepresentable implicit generic type arg.
//!
//! This test is based on gleam's `compiler_core::type_::expression::ExprTyper::check_let_exhaustiveness`,
//! which constructs the `Arena` type with the unrepresentable `Pattern` type as its generic arg
//! through inference from the `Compiler` type's corresponding field.

#![feature(decl_macro)]

#![allow(unused)]

struct Arena<T: Copy>([Option<T>; 128]);

impl<T: Copy> Arena<T> {
    pub fn new() -> Self {
        Self([None; 128])
    }
}

mod exhaustiveness {
    mod pattern {
        #[derive(Clone, Copy)]
        pub enum Pattern {
            Discard,
        }
    }

    use crate::Arena;
    use pattern::Pattern;

    const N: usize = 64;

    pub struct Compiler {
        patterns: Arena<Pattern>,
    }

    impl Compiler {
        pub fn new(patterns: Arena<Pattern>) -> Self {
            Self { patterns }
        }
    }
}

mod type_ {
    mod expression {
        use crate::Arena;
        use crate::exhaustiveness::Compiler;

        macro m() {
            fn check_exhaustiveness() {
                let _compiler = Compiler::new(Arena::new());
            }
        }

        m!();
    }
}
