//! Contiguous collections, indexed by specific [`Idx`] associted index types.
//!
//! The `Idx*` data structures are based on rustc's own [`rustc_index::Index*`][rustc_index] types.
//! The primary difference is that these `Idx*` versions have support for
//! serde serialization and deserialization
//! as their underlying vector and slice types respectively.
//!
//! [rustc_index]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_index/index.html

use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::slice::SliceIndex;

pub trait Idx: Copy + 'static + Eq + PartialEq + Debug + Hash {
    fn as_index(self) -> usize;
    fn from_index(idx: usize) -> Self;
}

pub trait IntoIdxSliceIndex<I: Idx, T: ?Sized> {
    type Output: SliceIndex<T>;
    fn into_idx_slice_index(self) -> Self::Output;
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for I {
    type Output = usize;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        self.as_index()
    }
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for std::ops::RangeFull {
    type Output = std::ops::RangeFull;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        self
    }
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for std::ops::Range<I> {
    type Output = std::ops::Range<usize>;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        std::ops::Range { start: self.start.as_index(), end: self.end.as_index() }
    }
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for std::ops::RangeInclusive<I> {
    type Output = std::ops::RangeInclusive<usize>;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        std::ops::RangeInclusive::new(self.start().as_index(), self.end().as_index())
    }
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for std::ops::RangeFrom<I> {
    type Output = std::ops::RangeFrom<usize>;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        std::ops::RangeFrom { start: self.start.as_index() }
    }
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for std::ops::RangeTo<I> {
    type Output = std::ops::RangeTo<usize>;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        std::ops::RangeTo { end: self.end.as_index() }
    }
}

impl<I: Idx, T> IntoIdxSliceIndex<I, [T]> for std::ops::RangeToInclusive<I> {
    type Output = std::ops::RangeToInclusive<usize>;
    #[inline]
    fn into_idx_slice_index(self) -> Self::Output {
        std::ops::RangeToInclusive { end: self.end.as_index() }
    }
}

pub struct IdxVec<I: Idx, T> {
    _index: PhantomData<fn(&I)>,
    raw: Vec<T>,
}

impl<I: Idx, T> IdxVec<I, T> {
    #[inline]
    fn from_raw(raw: Vec<T>) -> Self {
        Self { raw, _index: PhantomData }
    }

    #[inline]
    pub const fn new() -> Self {
        Self { raw: Vec::new(), _index: PhantomData }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self { raw: Vec::with_capacity(capacity), _index: PhantomData }
    }

    #[inline]
    pub fn as_slice(&self) -> &IdxSlice<I, T> {
        IdxSlice::from_raw(&self.raw)
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut IdxSlice<I, T> {
        IdxSlice::from_raw_mut(&mut self.raw)
    }

    #[inline]
    pub fn push(&mut self, value: T) -> I {
        let idx = self.next_index();
        self.raw.push(value);
        idx
    }

    #[inline]
    pub fn ensure_contains<F: FnMut() -> T>(&mut self, idx: I, fill_value: F) -> &mut T {
        let new_len = idx.as_index() + 1;
        if self.len() < new_len {
            self.raw.resize_with(new_len, fill_value);
        }

        &mut self[idx]
    }

    #[inline]
    pub fn into_iter(self) -> std::vec::IntoIter<T> {
        self.raw.into_iter()
    }

    #[inline]
    pub fn into_iter_enumerated(self) -> impl DoubleEndedIterator<Item = (I, T)> + ExactSizeIterator {
        self.raw.into_iter().enumerate().map(|(index, v)| (I::from_index(index), v))
    }
}

impl<I: Idx, T> IdxVec<I, Option<T>> {
    #[inline]
    pub fn insert(&mut self, idx: I, value: T) -> Option<T> {
        self.ensure_contains(idx, || None).replace(value)
    }

    #[inline]
    pub fn get_or_insert_with<F: FnOnce() -> T>(&mut self, idx: I, value: F) -> &mut T {
        self.ensure_contains(idx, || None).get_or_insert_with(value)
    }

    #[inline]
    pub fn remove(&mut self, idx: I) -> Option<T> {
        self.get_mut(idx)?.take()
    }
}

impl<I: Idx, T: Debug> Debug for IdxVec<I, T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.raw, fmt)
    }
}

impl<I: Idx, T: Clone> Clone for IdxVec<I, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self::from_raw(self.raw.clone())
    }

    #[inline]
    fn clone_from(&mut self, source: &Self) {
        self.raw.clone_from(&source.raw);
    }
}

impl<I: Idx, T> Deref for IdxVec<I, T> {
    type Target = IdxSlice<I, T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<I: Idx, T> DerefMut for IdxVec<I, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<I: Idx, T> Borrow<IdxSlice<I, T>> for IdxVec<I, T> {
    #[inline]
    fn borrow(&self) -> &IdxSlice<I, T> {
        self
    }
}

impl<I: Idx, T> BorrowMut<IdxSlice<I, T>> for IdxVec<I, T> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut IdxSlice<I, T> {
        self
    }
}

impl<I: Idx, T> Extend<T> for IdxVec<I, T> {
    #[inline]
    fn extend<O: IntoIterator<Item = T>>(&mut self, iter: O) {
        self.raw.extend(iter)
    }
}

impl<I: Idx, T> IntoIterator for IdxVec<I, T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.raw.into_iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IdxVec<I, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IdxVec<I, T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<I: Idx, T> Default for IdxVec<I, T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T: serde::Serialize> serde::Serialize for IdxVec<I, T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.raw.serialize(serializer)
    }
}

impl<'de, I: Idx, T: serde::Deserialize<'de>> serde::Deserialize<'de> for IdxVec<I, T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Vec::deserialize(deserializer).map(Self::from_raw)
    }
}

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IdxSlice<I: Idx, T> {
    _index: PhantomData<fn(&I)>,
    raw: [T],
}

impl<I: Idx, T> IdxSlice<I, T> {
    #[inline]
    const fn from_raw(raw: &[T]) -> &Self {
        let ptr: *const [T] = raw;
        // SAFETY: `IdxSlice` is `repr(transparent)` over a normal slice
        unsafe { &*(ptr as *const Self) }
    }

    #[inline]
    fn from_raw_mut(raw: &mut [T]) -> &mut Self {
        let ptr: *mut [T] = raw;
        // SAFETY: `IdxSlice` is `repr(transparent)` over a normal slice
        unsafe { &mut *(ptr as *mut Self) }
    }

    #[inline]
    pub const fn empty<'a>() -> &'a Self {
        Self::from_raw(&[])
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.raw.len()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    #[inline]
    pub fn next_index(&self) -> I {
        I::from_index(self.len())
    }

    #[inline]
    pub fn last_index(&self) -> Option<I> {
        self.len().checked_sub(1).map(I::from_index)
    }

    #[inline]
    pub fn indices(&self) -> impl DoubleEndedIterator<Item = I> + ExactSizeIterator {
        (0..self.len()).map(|index| I::from_index(index))
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.raw.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.raw.iter_mut()
    }

    #[inline]
    pub fn iter_enumerated(&self) -> impl DoubleEndedIterator<Item = (I, &T)> + ExactSizeIterator {
        self.raw.iter().enumerate().map(|(index, v)| (I::from_index(index), v))
    }

    #[inline]
    pub fn iter_enumerated_mut(&mut self) -> impl DoubleEndedIterator<Item = (I, &mut T)> + ExactSizeIterator {
        self.raw.iter_mut().enumerate().map(|(index, v)| (I::from_index(index), v))
    }

    #[inline]
    pub fn get<R: IntoIdxSliceIndex<I, [T]>>(&self, index: R) -> Option<&<R::Output as SliceIndex<[T]>>::Output> {
        self.raw.get(index.into_idx_slice_index())
    }

    #[inline]
    pub fn get_mut<R: IntoIdxSliceIndex<I, [T]>>(&mut self, index: R) -> Option<&mut <R::Output as SliceIndex<[T]>>::Output> {
        self.raw.get_mut(index.into_idx_slice_index())
    }
}

impl<I: Idx, T> IdxSlice<I, Option<T>> {
    #[inline]
    pub fn contains(&self, idx: I) -> bool {
        self.get(idx).is_some()
    }
}

impl<I: Idx, T: Debug> Debug for IdxSlice<I, T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.raw, fmt)
    }
}

impl<I: Idx, T, R: IntoIdxSliceIndex<I, [T]>> Index<R> for IdxSlice<I, T> {
    type Output = <R::Output as SliceIndex<[T]>>::Output;

    #[inline]
    fn index(&self, index: R) -> &Self::Output {
        &self.raw[index.into_idx_slice_index()]
    }
}

impl<I: Idx, T, R: IntoIdxSliceIndex<I, [T]>> IndexMut<R> for IdxSlice<I, T> {
    #[inline]
    fn index_mut(&mut self, index: R) -> &mut Self::Output {
        &mut self.raw[index.into_idx_slice_index()]
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IdxSlice<I, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.raw.iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IdxSlice<I, T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.raw.iter_mut()
    }
}

impl<I: Idx, T: Clone> ToOwned for IdxSlice<I, T> {
    type Owned = IdxVec<I, T>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        IdxVec::from_raw(self.raw.to_owned())
    }

    #[inline]
    fn clone_into(&self, target: &mut Self::Owned) {
        self.raw.clone_into(&mut target.raw)
    }
}

impl<I: Idx, T> Default for &IdxSlice<I, T> {
    #[inline]
    fn default() -> Self {
        IdxSlice::from_raw(Default::default())
    }
}

impl<I: Idx, T> Default for &mut IdxSlice<I, T> {
    #[inline]
    fn default() -> Self {
        IdxSlice::from_raw_mut(Default::default())
    }
}
