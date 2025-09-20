use mutest_emit::analysis::call_graph::{EntryPointAssoc, EntryPointAssocs, ExternEntryPoint, Target, TargetKind, TargetReachability, Unsafety};
use mutest_emit::analysis::hir;
use mutest_emit::codegen::symbols::Symbol;
use rustc_hash::FxHashMap;
use rustc_middle::bug;
use rustc_middle::ty::TyCtxt;

#[derive(Debug)]
pub struct RustcInvocation {
    pub args: Vec<String>,
    pub env_vars: Vec<(String, String)>,
}

#[derive(Debug)]
pub struct StableTarget {
    pub def_path_hash: hir::DefPathHash,
    pub unsafety: Unsafety,
    pub reachability: TargetReachability,
    pub reachable_from: FxHashMap<hir::DefPathHash, EntryPointAssoc>,
}

impl StableTarget {
    pub fn from_test_session<'tcx>(tcx: TyCtxt<'tcx>, target: &Target) -> StableTarget {
        let EntryPointAssocs::Local(reachable_from) = &target.reachable_from else {
            bug!("test compiler session generated targets with non-local entry points");
        };

        let stable_reachable_from = reachable_from.iter()
            .map(|(local_entry_point, &entry_point_assoc)| (tcx.def_path_hash(local_entry_point.local_def_id.to_def_id()), entry_point_assoc))
            .collect::<FxHashMap<_, _>>();

        StableTarget {
            def_path_hash: tcx.def_path_hash(target.def_id()),
            unsafety: target.unsafety,
            reachability: target.reachability,
            reachable_from: stable_reachable_from,
        }
    }

    pub fn into_target_session<'tcx>(&self, tcx: TyCtxt<'tcx>, path_strs: &FxHashMap<hir::DefPathHash, String>) -> Target {
        let session_reachable_from = self.reachable_from.iter()
            .map(|(&def_path_hash, &entry_point_assoc)| {
                let Some(path_str) = path_strs.get(&def_path_hash) else {
                    bug!("cannot find encoded path string for def path hash");
                };
                let extern_entry_point = ExternEntryPoint {
                    def_path_hash,
                    path_str: Symbol::intern(path_str),
                };
                (extern_entry_point, entry_point_assoc)
            })
            .collect::<FxHashMap<_, _>>();

        let Some(def_id) = tcx.def_path_hash_to_def_id(self.def_path_hash) else {
            tcx.dcx().fatal("cannot decode def path hash from another compilation session");
        };

        Target {
            kind: TargetKind::LocalMutable(def_id.expect_local()),
            unsafety: self.unsafety,
            reachability: self.reachability,
            reachable_from: EntryPointAssocs::Extern(session_reachable_from),
        }
    }
}

#[derive(Debug)]
pub struct ExternalTargets {
    pub stable_targets: Vec<StableTarget>,
    pub path_strs: FxHashMap<hir::DefPathHash, String>,
    pub json_definitions: FxHashMap<hir::DefPathHash, mutest_json::DefId>,
}

pub mod crate_const_storage;
pub mod recompilable_dep_crate;
pub mod specialized_crate;
