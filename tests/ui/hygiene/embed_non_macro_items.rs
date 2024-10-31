//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

struct Resolution<const IDX: isize>;

#[cfg(test)]
mod tests {
    #[test]
    fn test_embed_non_macro_items() {
        macro embed_non_macro_items($($item:item)+) {
            fn f() -> crate::Resolution<-1> { crate::Resolution }
            $($item)+
            fn g() -> crate::Resolution<-1> { crate::Resolution }
            mod inner {
                pub(super) fn h() -> crate::Resolution<-1> { crate::Resolution }
            }
        }

        embed_non_macro_items!(
            fn f() -> crate::Resolution<1> { crate::Resolution }
            fn g() -> crate::Resolution<2> { crate::Resolution }
            mod inner {
                pub(super) fn h() -> crate::Resolution<3> { crate::Resolution }
            }
        );

        let _: crate::Resolution<1> = f();
        let _: crate::Resolution<2> = g();
        let _: crate::Resolution<3> = inner::h();
    }
}
