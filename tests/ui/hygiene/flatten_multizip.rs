//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

//! The test macro `nested_macro_to_flatten_clousre` relies on macro hygiene
//! to get a different closure parameter ident on each recursion level
//! in a nested invocation to build up a multi-parameter closure to
//! flatten an N-ary tuple.
//!
//! This test also includes a more complex macro definition which
//! defines a generic wrapper around the std `into_iter` function, and
//! uses this local definition instead.
//!
//! This test is based on itertools's `izip` macro, see
//! https://docs.rs/itertools/latest/itertools/macro.izip.html.

#![feature(decl_macro)]

macro_rules! noop {
    ($expr:expr) => { $expr };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_flatten_multizip() {
        macro nested_macro_to_flatten_clousre {
            ($p:pat => $tup:expr) => { |$p| $tup },
            // The `b` identifier should be a different identifier on each recursion level because of hygiene.
            ($p:pat => ($($tup:tt)*), $_iter:expr $(, $tail:expr)*) => {
                nested_macro_to_flatten_clousre!(($p, b) => ($($tup)*, b) $(, $tail)*)
            },
        }

        macro zip_macro {
            ($first:expr $(,)*) => { std::iter::IntoIterator::into_iter($first) },
            ($first:expr $(, $rest:expr )* $(,)*) => {
                zip_macro!($first)
                    $(.zip($rest))*
                    .map(nested_macro_to_flatten_clousre!(a => (a) $(, $rest)*))
            },
        }

        mod inner {
            pub(super) fn d() -> impl Iterator<Item = usize> {
                10000..10010
            }
        }

        let a = 0..10;
        let b = 100..110;
        let c = 1000..1010;
        let _ = zip_macro!(a, b, noop!(c), inner::d());

        macro zip_macro_complex {
            ($first:expr $(,)*) => {
                {
                    mod inner {
                        pub(super) fn into_iter<T>(i: impl std::iter::IntoIterator<Item = T>) -> impl std::iter::Iterator<Item = T> {
                            std::iter::IntoIterator::into_iter(i)
                        }
                    }
                    inner::into_iter($first)
                }
            },
            ($first:expr $(, $rest:expr )* $(,)*) => {
                zip_macro_complex!($first)
                    $(.zip($rest))*
                    .map(nested_macro_to_flatten_clousre!(a => (a) $(, $rest)*))
            },
        }

        let a = 0..10;
        let b = 100..110;
        let c = 1000..1010;
        let _ = zip_macro_complex!(a, b, noop!(c), inner::d());

        // This must not be modified.
        let _: std::iter::Empty<usize> = std::iter::IntoIterator::into_iter(std::iter::empty::<usize>());
    }
}
