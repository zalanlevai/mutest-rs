pub mod ast_lowering;
pub mod diagnostic;
pub mod hir;
pub mod res;
pub mod tests;
pub mod ty;

pub trait Descr {
    fn descr(&self) -> &'static str;
}
