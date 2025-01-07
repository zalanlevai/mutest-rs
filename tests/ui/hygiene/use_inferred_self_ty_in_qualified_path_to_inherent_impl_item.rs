//@ build
//@ stderr: empty

//! This test shows that even for non-qualified associated item paths to inherent impl items,
//! it is sometimes necessary to use the inferred self type from the type system.
//!
//! This test is based on alacritty's `impl Default for Clipboard`,
//! although it is triggered by a combination of
//! alacritty's `Clipboard` struct's dyn trait indirection, and
//! copypasta's aliasing of the generic `X11ClipboardContext` type as
//! the non-generic `ClipboardContext` type (with the `x11` feature enabled).

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    struct X11Clipboard;
    struct X11Primary;

    struct X11ClipboardContext<S = X11Clipboard>(std::marker::PhantomData<S>);

    impl<S> X11ClipboardContext<S> {
        fn new() -> Result<X11ClipboardContext<S>, ()> {
            Ok(X11ClipboardContext(std::marker::PhantomData))
        }
    }

    // NOTE: Indirection through type alias is required for
    //       the `::new()` call with missing type arguments to be accepted in the first place,
    //       as the type alias changes the requirements on the type parameters.
    type ClipboardContext = X11ClipboardContext;

    // NOTE: Indirection through trait and `dyn` (in struct field) is required to
    //       trigger the compiler error related to the ambiguous type subpath
    //       with missing uninferrable type parameter.
    pub trait ClipboardProvider {}
    impl<S> ClipboardProvider for X11ClipboardContext<S> {}

    pub struct Clipboard {
        clipboard: Box<dyn ClipboardProvider>,
        selection: Option<Box<dyn ClipboardProvider>>,
    }

    impl Default for Clipboard {
        fn default() -> Self {
            Self {
                // NOTE: Without resolving the type of `ClipboardContext`, this path segment
                //       gets resolved to the underlying `X11ClipboardContext` definition,
                //       and the resulting sanitized path triggers a "type annotations needed" error, with
                //       "cannot infer type of the type parameter `S` declared on the struct `X11ClipboardContext`".
                clipboard: Box::new(ClipboardContext::new().unwrap()),
                // NOTE: This works fine, since the type parameter is explicitly provided.
                selection: Some(Box::new(X11ClipboardContext::<X11Primary>::new().unwrap())),
            }
        }
    }
}

#[test]
fn test() {
    m!();
}
