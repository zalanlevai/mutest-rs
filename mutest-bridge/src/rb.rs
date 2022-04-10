use std::ops::Deref;

#[derive(Debug)]
pub enum Rb<'a, T: 'a + ?Sized> {
    Ref(&'a T),
    Box(Box<T>),
}

impl<'a, T: 'a + ?Sized> From<&'a T> for Rb<'a, T> {
    fn from(r: &'a T) -> Self {
        Self::Ref(r)
    }
}

impl<'a, T: 'a + ?Sized> From<Box<T>> for Rb<'a, T> {
    fn from(b: Box<T>) -> Self {
        Self::Box(b)
    }
}

impl<'a, T: 'a + ?Sized> Rb<'a, T> {
    pub fn from_ref(r: &'a T) -> Self {
        Self::Ref(r)
    }

    pub fn from_box(b: Box<T>) -> Self {
        Self::Box(b)
    }
}

impl<'a, T: 'a> Rb<'a, T> {
    pub fn new_box(v: T) -> Self {
        Self::Box(Box::new(v))
    }
}

impl<'a, T: 'a + ?Sized> Deref for Rb<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Ref(r) => r,
            Self::Box(b) => b,
        }
    }
}
