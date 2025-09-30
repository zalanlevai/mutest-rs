use std::collections::HashSet;

use crate::harness::ActiveMutantHandle;

pub macro static_map($($input:tt)*) {
    {
        // NOTE: The `phf` crate name must exist in this generated scope
        //       for the `phf_map` macro to expand correctly.
        extern crate __mutest_runtime_public_dep_phf as phf;
        phf::phf_map!($($input)*)
    }
}

pub type TestPath = &'static str;

#[derive(Debug)]
pub struct ExternalTestsExtra {
    pub test_crate_name: &'static str,
}

#[derive(Debug)]
pub enum TestSuite {
    Tests(&'static [&'static ::test::TestDescAndFn], Option<&'static ExternalTestsExtra>),
}

pub fn reachable_tests(mutation: &MutationMeta, external_tests_extra: Option<&ExternalTestsExtra>) -> HashSet<TestPath> {
    match (&mutation.reachable_from, external_tests_extra) {
        (EntryPoints::InternalTests(reachable_from), None) => reachable_from.keys().copied().collect(),
        (EntryPoints::InternalTests(_), _) => panic!("encountered meta-mutant compiled for internal tests being run against external tests"),

        (EntryPoints::ExternalTests(reachable_from), Some(_external_tests_extra)) => reachable_from.keys().copied().collect(),
        (EntryPoints::ExternalTests(_), _) => panic!("encounteref meta-mutant compiled for external tests being run without external test metadata"),
    }
}

pub fn reachable_tests_count(mutation: &MutationMeta, external_tests_extra: Option<&ExternalTestsExtra>) -> usize {
    match (&mutation.reachable_from, external_tests_extra) {
        (EntryPoints::InternalTests(reachable_from), None) => reachable_from.len(),
        (EntryPoints::InternalTests(_), _) => panic!("encountered meta-mutant compiled for internal tests being run against external tests"),

        (EntryPoints::ExternalTests(reachable_from), Some(_external_tests_extra)) => reachable_from.len(),
        (EntryPoints::ExternalTests(_), _) => panic!("encounteref meta-mutant compiled for external tests being run without external test metadata"),
    }
}

pub fn test_reachability(mutation: &MutationMeta, test_path: &str, external_tests_extra: Option<&ExternalTestsExtra>) -> Option<usize> {
    match (&mutation.reachable_from, external_tests_extra) {
        (EntryPoints::InternalTests(reachable_from), None) => reachable_from.get(test_path).copied(),
        (EntryPoints::InternalTests(_), _) => panic!("encountered meta-mutant compiled for internal tests being run against external tests"),

        (EntryPoints::ExternalTests(reachable_from), Some(_external_tests_extra)) => reachable_from.get(test_path).copied(),
        (EntryPoints::ExternalTests(_), _) => panic!("encounteref meta-mutant compiled for external tests being run without external test metadata"),
    }
}

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
pub enum EntryPoints {
    InternalTests(phf::Map<TestPath, usize>),
    ExternalTests(phf::Map<TestPath, usize>),
}

#[derive(Debug)]
pub struct MutationMeta {
    pub id: u32,
    pub safety: MutationSafety,
    pub op_name: &'static str,
    pub display_name: &'static str,
    pub display_location: &'static str,
    pub reachable_from: EntryPoints,
    pub undetected_diagnostic: &'static str,
}

impl MutationMeta {
    pub fn is_unsafe(&self) -> bool {
        !matches!(self.safety, MutationSafety::Safe)
    }
}

#[derive(Debug)]
pub struct StandaloneMutantMeta<S: SubstMap + 'static> {
    pub mutation: &'static MutationMeta,
    pub substitutions: &'static S,
}

#[derive(Debug)]
pub struct BatchedMutantMeta<S: SubstMap + 'static> {
    pub batch_id: u32,
    pub mutations: &'static [&'static MutationMeta],
    pub substitutions: &'static S,
}

#[derive(Copy, Clone)]
pub enum Mutant<S: SubstMap + 'static> {
    Mutation(&'static StandaloneMutantMeta<S>),
    Batch(&'static BatchedMutantMeta<S>),
}

impl<S: SubstMap + 'static> Mutant<S> {
    pub fn substitutions(&self) -> &'static S {
        match self {
            Self::Mutation(mutant) => mutant.substitutions,
            Self::Batch(mutant) => mutant.substitutions,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum MutationParallelism<S: SubstMap + 'static> {
    None(&'static [StandaloneMutantMeta<S>]),
    Batched(&'static [BatchedMutantMeta<S>]),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum CargoTargetKind {
    Lib,
    MainBin,
    Bin,
    Example,
    Test,
}

#[derive(Debug)]
pub struct MetaMutant<S: SubstMap + 'static> {
    pub cargo_package_name: Option<&'static str>,
    pub cargo_target_kind: Option<CargoTargetKind>,
    pub crate_name: &'static str,

    pub active_mutant_handle: &'static ActiveMutantHandle<S>,
    pub mutations: &'static [&'static MutationMeta],
    pub mutation_parallelism: MutationParallelism<S>,
}

impl<S: SubstMap + 'static> MetaMutant<S> {
    pub fn mutants(&self) -> Box<dyn Iterator<Item = Mutant<S>>> {
        match self.mutation_parallelism {
            MutationParallelism::None(mutants) => Box::new(mutants.iter().map(|mutant| Mutant::Mutation(mutant))),
            MutationParallelism::Batched(mutants) => Box::new(mutants.iter().map(|mutant| Mutant::Batch(mutant))),
        }
    }

    pub fn find_mutant_with_mutation(&self, mutation_id: u32) -> Option<Mutant<S>> {
        self.mutants().find(|mutant| {
            match mutant {
                Mutant::Mutation(mutant) => mutant.mutation.id == mutation_id,
                Mutant::Batch(mutant) => mutant.mutations.iter().any(|mutation| mutation.id == mutation_id),
            }
        })
    }
}
