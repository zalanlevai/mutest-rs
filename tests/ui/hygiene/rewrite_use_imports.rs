//@ build
//@ stderr: empty
//@ aux-build: dummy_crate.rs
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

extern crate dummy_crate;

pub trait TopLevelItemInTypeNs {}
pub const TOP_LEVEL_ITEM_IN_VALUE_NS: usize = 1;
pub macro top_level_item_in_macro_ns() {}

macro m() {
    // TEST: Top-level, individual item imports.
    mod test_top_level_invidiual_item_imports {
        use inner::InTypeNs;
        use inner::IN_VALUE_NS;
        use inner::in_macro_ns;
        use inner::nested_mod::within;
        use inner::nested_mod::within::Item;
        use inner::nested_mod::within::Type;

        mod inner {
            pub trait InTypeNs {}
            pub const IN_VALUE_NS: usize = 1;
            pub macro in_macro_ns() {}

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                    pub type Type = ();
                }
            }
        }
    }

    // TEST: Item imports with discard aliases.
    mod test_discarded_item_imports {
        use inner::InTypeNs as _;
        use inner::nested_mod::within::Item as _;

        mod inner {
            pub trait InTypeNs {}

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                }
            }
        }
    }

    // TEST: Item imports with used aliases.
    mod test_aliased_item_imports {
        use inner::InTypeNs as InTypeNsRenamed;
        use inner::nested_mod::within::Item as ItemRenamed;

        mod inner {
            pub trait InTypeNs {}

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                }
            }
        }

        struct Test(ItemRenamed);
        impl InTypeNsRenamed for Test {}
    }

    // TEST: Item imports with nested use trees.
    mod test_nested_use_tree_imports {
        use inner::{InTypeNs, IN_VALUE_NS, in_macro_ns, nested_mod::within::{Item, Type}};

        mod inner {
            pub trait InTypeNs {}
            pub const IN_VALUE_NS: usize = 1;
            pub macro in_macro_ns() {}

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                    pub type Type = ();
                }
            }
        }
    }

    // TEST: Item imports with top-level "nested" use tree.
    mod test_top_level_nested_use_tree_imports {
        use {
            inner::{InTypeNs, IN_VALUE_NS, in_macro_ns},
            inner::nested_mod::within::{Item, Type},
        };

        mod inner {
            pub trait InTypeNs {}
            pub const IN_VALUE_NS: usize = 1;
            pub macro in_macro_ns() {}

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                    pub type Type = ();
                }
            }
        }
    }

    // TEST: Glob imports.
    mod test_glob_imports {
        use inner::*;
        use inner::nested_mod::*;
        use inner::nested_mod::within::*;

        mod inner {
            pub trait InTypeNs {}
            pub const IN_VALUE_NS: usize = 1;
            pub macro in_macro_ns() {}

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                    pub type Type = ();
                }
            }
        }
    }

    // TEST: `self` module imports.
    mod test_self_mod_imports {
        use self::inner as self_inner;
        use inner::nested_mod::{self, within::{self}};
        // NOTE: `self` can only appear as the root or the ending path segment, not in the middle of the path.

        mod inner {
            pub mod nested_mod {
                pub mod within {}
            }
        }
    }

    // TEST: `super` module imports.
    mod test_super_mod_imports {
        mod inner {
            use super::{inner, inner::{nested_mod as nested_mod_renamed, nested_mod::within}};
            use super::*;
            use super::super::*;
            // NOTE: `super` can only appear as the root path segment, not in the middle or at the end of the path.

            pub mod nested_mod {
                pub struct Item;

                pub mod within {
                    use super::super::nested_mod;

                    pub mod further {
                        use super::super::Item;
                    }
                }
            }
        }
    }

    // TEST: Item imports of re-exported items.
    mod test_reexported_item_imports {
        use inner::{InTypeNsReexported, IN_VALUE_NS_REEXPORTED, in_macro_ns_reexported};
        use inner::{ItemReexportedReexported, TypeReexportedReexported};
        use inner::within_reexported::{ItemReexported, TypeReexported};

        mod inner {
            pub trait InTypeNs {}
            pub const IN_VALUE_NS: usize = 1;
            pub macro in_macro_ns() {}

            pub use {InTypeNs as InTypeNsReexported, IN_VALUE_NS as IN_VALUE_NS_REEXPORTED, in_macro_ns as in_macro_ns_reexported};
            pub use nested_mod::{ItemReexported as ItemReexportedReexported, TypeReexported as TypeReexportedReexported};
            pub use nested_mod::within as within_reexported;

            pub use self::InTypeNs as InTypeNsRenamed;

            pub mod nested_mod {
                pub mod within {
                    pub struct Item;
                    pub type Type = ();

                    pub use {Item as ItemReexported, Type as TypeReexported};
                }

                pub use within::*;
            }
        }
    }

    // TEST: Item imports with diverging resolutions.
    mod test_diverging_res_item_imports {
        use inner::Item;

        mod inner {
            #[allow(non_upper_case_globals)]
            pub const Item: usize = 1;

            pub use crate::TopLevelItemInTypeNs as Item;
            pub use crate::top_level_item_in_macro_ns as Item;
        }

        struct Test([(); Item]);
        impl Item for Test {}
        Item!();
    }

    // TEST: Item imports from other macro scopes.
    mod test_item_imports_from_macros {
        use inner::MixedMacroRulesReexports;

        mod inner {
            pub use crate::TopLevelItemInTypeNs as MixedMacroRulesReexports;

            macro_rules! n {
                () => {
                    pub use crate::TOP_LEVEL_ITEM_IN_VALUE_NS as MixedMacroRulesReexports;
                    pub use crate::top_level_item_in_macro_ns as MixedMacroRulesReexports;
                };
            }
            n!();
        }
    }

    // TEST: Item imports from implicit extern crates.
    mod test_item_imports_from_extern_crates {
        use {core, std};
        use core::marker::PhantomData;
    }

    // TEST: Item imports from global paths.
    mod test_global_path_item_imports {
        use ::{core, std};
        use ::std::collections::HashMap;

    }

    // TEST: Relative item imports in macro definitions.
    mod test_relative_item_imports_in_macro_defs {
        mod def {
            pub fn f() {}

            pub mod inner {
                pub mod within {
                    fn g() {}
                }

                pub macro m() {
                    use super::f;
                    use super::super::super::*;

                    use self::within::*;
                    use within::*;

                    use within_macro::EnsureLocalItemsAreStillAccessible;
                    mod within_macro {
                        pub struct EnsureLocalItemsAreStillAccessible;
                    }
                }
            }
        }

        def::inner::m!();
    }

    // TEST: Defining-crate-local imports in macro_rules macro definitions.
    mod test_dollar_crate_imports {
        macro_rules! n {
            () => {
                use $crate::TopLevelItemInTypeNs;
            };
        }
        n!();
    }

    // TEST: Extern crate imports.
    mod test_crate_imports {
        use dummy_crate as dummy_crate_renamed;
        use ::dummy_crate as global_dummy_crate;
        use dummy_crate::{foo, bar};
        use ::dummy_crate::{foo as foo_renamed, bar as bar_renamed};
    }

    // TEST: Reexport of local macro_rules macro item.
    mod test_macro_rules_reexports {
        macro_rules! cstr { () => {}; }
        pub(crate) use cstr;
    }
}

m!();
