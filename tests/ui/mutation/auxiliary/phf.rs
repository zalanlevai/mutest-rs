#![crate_type = "lib"]

#![feature(decl_macro)]

pub fn incompatible_internal_phf_map_new() {}

pub macro incompatible_phf_map() {
    incompatible_internal_phf_map_new();
}
