pub macro static_map($($input:tt)*) {
    {
        // NOTE: The `phf` crate name must exist in this generated scope
        //       for the `phf_map` macro to expand correctly.
        extern crate __mutest_runtime_public_dep_phf as phf;
        phf::phf_map!($($input)*)
    }
}

pub type TestPath = &'static str;

pub type SubstLocIdx = usize;

pub trait SubstMap: Sized + Clone {
    fn subst_at(&self, subst_loc_idx: SubstLocIdx) -> Option<SubstMeta>;

    /// # Safety
    ///
    /// The substitution location index must be valid for the substitution map.
    unsafe fn subst_at_unchecked(&self, subst_loc_idx: SubstLocIdx) -> Option<SubstMeta>;
}

impl<const N: usize> SubstMap for [Option<SubstMeta>; N] {
    #[inline]
    fn subst_at(&self, subst_loc_idx: SubstLocIdx) -> Option<SubstMeta> {
        self[subst_loc_idx]
    }

    #[inline]
    unsafe fn subst_at_unchecked(&self, subst_loc_idx: SubstLocIdx) -> Option<SubstMeta> {
        // SAFETY: The caller must ensure that the substitution location index is
        //         valid for the active substitution map.
        unsafe { *self.get_unchecked(subst_loc_idx) }
    }
}

// NOTE: This function must be a standalone function not on the SubstMap trait (and corresponding impl)
//       until const associated functions are implemented.
pub const fn subst_map_array<const N: usize>(substs: &[(SubstLocIdx, SubstMeta)]) -> [Option<SubstMeta>; N] {
    let mut subst_map = [None; N];

    // NOTE: We must use a manual index-based loop, because
    //       for-loops and iterators are not yet supported in const contexts.
    let mut i = 0;
    while i < substs.len() {
        let (subst_loc_idx, subst) = substs[i];
        subst_map[subst_loc_idx] = Some(subst);

        i += 1;
    }

    subst_map
}

#[derive(Clone, Copy, Debug)]
pub struct SubstMeta {
    pub mutation: &'static MutationMeta,
}

#[derive(Debug)]
pub enum MutationSafety {
    Safe,
    Tainted,
    Unsafe,
}

#[derive(Debug)]
pub struct MutationMeta {
    pub id: u32,
    pub safety: MutationSafety,
    pub op_name: &'static str,
    pub display_name: &'static str,
    pub display_location: &'static str,
    pub reachable_from: phf::Map<TestPath, usize>,
    pub undetected_diagnostic: &'static str,
}

#[derive(Debug)]
pub struct MutantMeta<S: SubstMap + 'static> {
    pub id: u32,
    pub mutations: &'static [&'static MutationMeta],
    pub substitutions: &'static S,
}

impl<S: SubstMap> MutantMeta<S> {
    pub fn is_unsafe(&self) -> bool {
        self.mutations.iter().any(|m| !matches!(m.safety, MutationSafety::Safe))
    }
}
