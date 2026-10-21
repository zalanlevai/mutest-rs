#![crate_type = "lib"]

extern crate private_dep;

#[derive(Default)]
pub struct ProxyStruct(pub usize);

pub fn proxy_struct(v: usize) -> ProxyStruct {
    ProxyStruct(v)
}

pub fn private_dep_struct(v: usize) -> private_dep::PrivateDepStruct {
    private_dep::PrivateDepStruct(v)
}
