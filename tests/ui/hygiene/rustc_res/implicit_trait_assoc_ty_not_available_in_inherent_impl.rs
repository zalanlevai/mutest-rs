//@ build: fail
//@ stderr
//@ mutest-flags: --Zno-sanitize-macro-expns

use core::ops::Deref;

struct DerefOnly(u32);

impl Deref for DerefOnly {
    type Target = u32;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl DerefOnly {
    fn copy(&self) -> Self::Target { self.0 }
}
