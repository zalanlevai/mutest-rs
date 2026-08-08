#![crate_type = "lib"]

#![feature(decl_macro)]

// NOTE: Only the following three attributes have been "made" unsafe,
//       all other unsafe attributes were unsafe when they were stabilized.
//       See https://doc.rust-lang.org/edition-guide/rust-2024/unsafe-attributes.html.
pub macro unmarked() {
    #[export_name = "exported_symbol"]
    fn internal_symbol() {}

    #[cfg_attr(not(target_vendor = "apple"), link_section = ".rodata")]
    #[cfg_attr(target_vendor = "apple", link_section = "__TEXT,__const")]
    const READ_ONLY: () = ();

    #[no_mangle]
    fn no_mangle() {}
}

// NOTE: Unsafe attributes can be marked unsafe in any edition,
//       but the above three are only required to be marked unsafe starting in edition 2024.
pub macro marked() {
    #[unsafe(export_name = "exported_symbol")]
    fn internal_symbol() {}

    #[cfg_attr(not(target_vendor = "apple"), unsafe(link_section = ".rodata"))]
    #[cfg_attr(target_vendor = "apple", unsafe(link_section = "__TEXT,__const"))]
    const READ_ONLY: () = ();

    #[unsafe(no_mangle)]
    fn no_mangle() {}
}
