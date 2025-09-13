use std::cell::UnsafeCell;
use std::collections::hash_map;
use std::hash::{Hash, Hasher};
use std::iter;

use rustc_hash::{FxHashSet, FxHashMap};
use rustc_middle::mir;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use smallvec::{SmallVec, smallvec};

use crate::analysis::ast_lowering;
use crate::analysis::hir;
use crate::analysis::res;
use crate::analysis::tests::{self, Test};
use crate::analysis::ty::{self, TyCtxt};
use crate::codegen::ast;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::mutation::UnsafeTargeting;
use crate::codegen::symbols::{DUMMY_SP, Span, span_diagnostic_ord, sym};
use crate::codegen::tool_attr;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnsafeSource {
    EnclosingUnsafe,
    Unsafe,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Unsafety {
    None,
    /// Safe code called from an unsafe context.
    Tainted(UnsafeSource),
    Unsafe(UnsafeSource),
}

impl Unsafety {
    pub fn any(&self) -> bool {
        !matches!(self, Self::None)
    }

    pub fn is_unsafe(&self, unsafe_targeting: UnsafeTargeting) -> bool {
        matches!((unsafe_targeting, self),
            | (_, Unsafety::Unsafe(UnsafeSource::Unsafe) | Unsafety::Tainted(UnsafeSource::Unsafe))
            | (UnsafeTargeting::None, Unsafety::Unsafe(_) | Unsafety::Tainted(_))
            | (UnsafeTargeting::OnlyEnclosing(hir::Safety::Unsafe), Unsafety::Unsafe(UnsafeSource::EnclosingUnsafe) | Unsafety::Tainted(UnsafeSource::EnclosingUnsafe))
        )
    }
}

struct BodyUnsafetyChecker {
    unsafety: Option<Unsafety>,
}

impl<'ast> ast::visit::Visitor<'ast> for BodyUnsafetyChecker {
    fn visit_block(&mut self, block: &'ast ast::Block) {
        if let ast::BlockCheckMode::Unsafe(ast::UnsafeSource::UserProvided) = block.rules {
            self.unsafety = Some(Unsafety::Unsafe(UnsafeSource::EnclosingUnsafe));
            return;
        }

        ast::visit::walk_block(self, block);
    }
}

fn check_item_unsafety<'ast>(item: ast::DefItem<'ast>) -> Unsafety {
    let ast::DefItemKind::Fn(target_fn) = item.kind() else { return Unsafety::None };

    let (ast::Safety::Default | ast::Safety::Safe(_)) = target_fn.sig.header.safety else { return Unsafety::Unsafe(UnsafeSource::Unsafe) };

    let Some(target_body) = &target_fn.body else { return Unsafety::None };
    let mut checker = BodyUnsafetyChecker { unsafety: None };
    checker.visit_block(target_body);
    checker.unsafety.unwrap_or(Unsafety::None)
}

fn collect_unsafe_blocks<'tcx>(body_hir: &'tcx hir::Body<'tcx>, root_scope_safety: hir::Safety) -> Vec<&'tcx hir::Block<'tcx>> {
    struct BodyUnsafeBlockCollector<'tcx> {
        current_scope_safety: hir::Safety,
        unsafe_blocks: Vec<&'tcx hir::Block<'tcx>>,
    }

    impl<'tcx> hir::intravisit::Visitor<'tcx> for BodyUnsafeBlockCollector<'tcx> {
        fn visit_block(&mut self, block: &'tcx hir::Block<'tcx>) {
            let previous_scope_unsafety = self.current_scope_safety;
            self.current_scope_safety = match block.rules {
                | hir::BlockCheckMode::DefaultBlock
                // NOTE: We explicitly ignore compiler-generated unsafe blocks.
                | hir::BlockCheckMode::UnsafeBlock(hir::UnsafeSource::CompilerGenerated)
                => self.current_scope_safety,

                hir::BlockCheckMode::UnsafeBlock(hir::UnsafeSource::UserProvided) => hir::Safety::Unsafe,
            };

            if self.current_scope_safety == hir::Safety::Unsafe {
                self.unsafe_blocks.push(block);
            }

            hir::intravisit::walk_block(self, block);

            self.current_scope_safety = previous_scope_unsafety;
        }
    }

    let mut collector = BodyUnsafeBlockCollector {
        current_scope_safety: root_scope_safety,
        unsafe_blocks: vec![],
    };
    hir::intravisit::Visitor::visit_body(&mut collector, body_hir);
    collector.unsafe_blocks
}

#[derive(Copy, Clone, Debug)]
pub struct EntryPoint {
    pub def_id: hir::LocalDefId,
}

impl EntryPoint {
    pub fn path_str<'tcx>(&self, tcx: TyCtxt<'tcx>) -> String {
        res::def_id_path(tcx, self.def_id.to_def_id()).iter()
            .skip(1) // Skip crate name in entry point path strings.
            .filter_map(|&segment_def_id| tcx.opt_item_name(segment_def_id).map(|symbol| symbol.as_str().to_owned()))
            .intersperse("::".to_owned())
            .collect::<String>()
    }
}

impl Eq for EntryPoint {}
impl PartialEq for EntryPoint {
    fn eq(&self, other: &Self) -> bool {
        self.def_id == other.def_id
    }
}

impl Hash for EntryPoint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TargetKind {
    LocalMutable(hir::LocalDefId),
    ExternEntryPoint(hir::DefId),
}

#[derive(Copy, Clone, Debug)]
pub enum TargetReachability {
    DirectEntry,
    NestedCallee { distance: usize },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EntryPointAssociation {
    pub distance: usize,
    pub unsafe_call_path: Option<UnsafeSource>,
}

#[derive(Debug)]
pub struct Target {
    pub kind: TargetKind,
    pub unsafety: Unsafety,
    pub reachability: TargetReachability,
    pub reachable_from: FxHashMap<EntryPoint, EntryPointAssociation>,
}

impl Target {
    pub fn def_id(&self) -> hir::DefId {
        match self.kind {
            TargetKind::LocalMutable(local_def_id) => local_def_id.to_def_id(),
            TargetKind::ExternEntryPoint(def_id) => def_id,
        }
    }

    pub fn is_tainted(&self, entry_point: EntryPoint, unsafe_targeting: UnsafeTargeting) -> bool {
        self.reachable_from.get(&entry_point).is_some_and(|entry_point| {
            let unsafety = entry_point.unsafe_call_path.map(Unsafety::Tainted).unwrap_or(Unsafety::None);
            unsafety.is_unsafe(unsafe_targeting)
        })
    }
}

/// All functions we can introduce mutations in.
/// Does not include closures, as they are (currently) considered part of their containing function, rather than
/// standalone functions. This might change in the future.
pub fn all_mutable_fns<'tcx, 'tst>(tcx: TyCtxt<'tcx>, tests: &'tst [Test]) -> impl Iterator<Item = hir::LocalDefId> + 'tcx {
    let entry_fn = tcx.entry_fn(());
    let test_def_ids = tests.iter().map(|test| test.def_id).collect::<FxHashSet<_>>();

    tcx.hir_crate_items(()).definitions()
        .filter(move |&local_def_id| {
            let def_id = local_def_id.to_def_id();
            let hir_id = tcx.local_def_id_to_hir_id(local_def_id);

            // TODO: Ignore #[coverage(off)] functions
            matches!(tcx.def_kind(def_id), hir::DefKind::Fn | hir::DefKind::AssocFn)
                // fn main() {}
                && !entry_fn.map(|(entry_def_id, _)| def_id == entry_def_id).unwrap_or(false)
                // const fn
                && !tcx.is_const_fn(def_id)
                // fn;
                && !tcx.hir_node_by_def_id(local_def_id).body_id().is_none()
                // #[test] functions, or inner functions
                && !test_def_ids.contains(&local_def_id)
                && !res::parent_iter(tcx, def_id).any(|parent_id| parent_id.as_local().is_some_and(|local_parent_id| test_def_ids.contains(&local_parent_id)))
                // #[cfg(test)] functions, or functions in #[cfg(test)] module
                && !tests::is_marked_or_in_cfg_test(tcx, hir_id)
                // #[mutest::skip] functions
                && !tool_attr::skip(tcx.hir_attrs(hir_id))
        })
}

pub fn all_public_interface_fns<'tcx>(tcx: TyCtxt<'tcx>) -> impl Iterator<Item = hir::LocalDefId> {
    tcx.hir_crate_items(()).definitions()
        .filter(move |&local_def_id| {
            let def_id = local_def_id.to_def_id();

            matches!(tcx.def_kind(def_id), hir::DefKind::Fn | hir::DefKind::AssocFn)
                // fn;
                && !tcx.hir_node_by_def_id(local_def_id).body_id().is_none()
                // pub(crate) fn;
                && tcx.visibility(local_def_id).is_public()
        })
}

#[derive(Clone, Copy, Debug)]
pub enum EntryPoints<'a> {
    Tests(&'a [Test]),
    PublicInterface(&'a [hir::LocalDefId]),
}

impl<'a> EntryPoints<'a> {
    pub fn iter(&self) -> Box<dyn Iterator<Item = EntryPoint> + 'a> {
        match self {
            EntryPoints::Tests(tests) => {
                let iter = tests.iter()
                    .filter(|test| !test.ignore)
                    .map(|test| EntryPoint { def_id: test.def_id });
                Box::new(iter)
            },
            EntryPoints::PublicInterface(def_ids) => Box::new(def_ids.iter().map(|&def_id| EntryPoint { def_id })),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CallKind<'tcx> {
    Def(hir::DefId, ty::GenericArgsRef<'tcx>),
    Ptr(ty::PolyFnSig<'tcx>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Call<'tcx> {
    pub kind: CallKind<'tcx>,
    pub safety: hir::Safety,
    pub span: Span,
}

// Based on `rustc_mir_transform::inline::cycle::mir_inliner_callees`.
pub fn mir_callees<'tcx>(tcx: TyCtxt<'tcx>, body_mir: &'tcx mir::Body<'tcx>, generic_args: ty::GenericArgsRef<'tcx>) -> impl Iterator<Item = Call<'tcx>> {
    let instance = ty::Instance { def: body_mir.source.instance, args: generic_args };
    let typing_env = ty::TypingEnv::fully_monomorphized();

    let body_def_id = body_mir.source.instance.def_id();
    let body_safety = match tcx.def_kind(body_def_id) {
        hir::DefKind::Closure => hir::Safety::Safe,
        _ => tcx.fn_sig(body_def_id).skip_binder().safety(),
    };

    let body_hir = body_def_id.as_local()
        .and_then(|local_def_id| tcx.hir_node_by_def_id(local_def_id).body_id())
        .map(|body_id| tcx.hir_body(body_id));
    let unsafe_blocks = body_hir.map(|body_hir| collect_unsafe_blocks(body_hir, body_safety));

    body_mir.basic_blocks.iter()
        .filter_map(move |basic_block| {
            let terminator = basic_block.terminator();
            let mir::TerminatorKind::Call { func, args: call_args, .. } = &terminator.kind else { return None; };

            let ty = func.ty(&body_mir.local_decls, tcx);
            let span = terminator.source_info.span;

            let mut safety = body_safety;

            if safety != hir::Safety::Unsafe {
                if let Some(unsafe_blocks) = &unsafe_blocks {
                    if unsafe_blocks.iter().any(|unsafe_block| span.find_ancestor_inside_same_ctxt(unsafe_block.span).is_some()) {
                        safety = hir::Safety::Unsafe;
                    }
                }
            }

            match ty.kind() {
                &ty::TyKind::FnDef(mut def_id, mut generic_args) => {
                    if tcx.is_intrinsic(def_id, sym::const_eval_select) {
                        let func = &call_args[2].node;
                        let ty = func.ty(&body_mir.local_decls, tcx);
                        let &ty::TyKind::FnDef(inner_def_id, inner_generic_args) = ty.kind() else { return None; };

                        def_id = inner_def_id;
                        generic_args = inner_generic_args;
                    }

                    let generic_args = instance.instantiate_mir_and_normalize_erasing_regions(tcx, typing_env, ty::EarlyBinder::bind(generic_args));

                    if safety != hir::Safety::Unsafe {
                        safety = tcx.fn_sig(def_id).skip_binder().safety();
                    }

                    Some(Call { kind: CallKind::Def(def_id, generic_args), safety, span })
                }

                &ty::TyKind::FnPtr(fn_sig_tys, fn_header) => {
                    let fn_sig_tys = instance.instantiate_mir_and_normalize_erasing_regions(tcx, typing_env, ty::EarlyBinder::bind(fn_sig_tys));

                    if safety != hir::Safety::Unsafe {
                        safety = fn_header.safety;
                    }

                    Some(Call { kind: CallKind::Ptr(fn_sig_tys.with(fn_header)), safety, span })
                }

                _ => None,
            }
        })
}

pub fn drop_glue_callees<'tcx>(tcx: TyCtxt<'tcx>, body_mir: &'tcx mir::Body<'tcx>, generic_args: ty::GenericArgsRef<'tcx>) -> impl Iterator<Item = Call<'tcx>> {
    let instance = ty::Instance { def: body_mir.source.instance, args: generic_args };
    let typing_env = ty::TypingEnv::fully_monomorphized();

    body_mir.mentioned_items.iter().flatten()
        .filter_map(|mentioned_item| {
            match &mentioned_item.node {
                mir::MentionedItem::Drop(dropped_ty) => Some(dropped_ty),
                _ => None,
            }
        })
        .map(move |&dropped_ty| {
            let dropped_ty = instance.instantiate_mir_and_normalize_erasing_regions(tcx, typing_env, ty::EarlyBinder::bind(dropped_ty));
            ty::Instance::resolve_drop_in_place(tcx, dropped_ty)
        })
        .flat_map(move |drop_in_place| tcx.mir_inliner_callees(drop_in_place.def))
        .map(move |&(def_id, generic_args)| {
            let safety = tcx.fn_sig(def_id).skip_binder().safety();
            Call { kind: CallKind::Def(def_id, generic_args), safety, span: DUMMY_SP }
        })
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Callee<'tcx> {
    pub def_id: hir::DefId,
    pub generic_args: ty::GenericArgsRef<'tcx>,
}

impl<'tcx> Callee<'tcx> {
    pub fn new(def_id: hir::DefId, generic_args: ty::GenericArgsRef<'tcx>) -> Self {
        Self { def_id, generic_args }
    }

    pub fn display_str(&self, tcx: TyCtxt<'tcx>) -> String {
        tcx.def_path_str_with_args(self.def_id, self.generic_args)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InstanceCall<'tcx> {
    pub callee: Callee<'tcx>,
    pub safety: hir::Safety,
    pub span: Span,
}

pub struct CallTrace<'tcx> {
    pub root: hir::LocalDefId,
    pub nested_calls: SmallVec<[Callee<'tcx>; 1]>,
}

impl<'tcx> CallTrace<'tcx> {
    pub fn contains(&self, callee: Callee<'tcx>) -> bool {
        self.nested_calls.iter().any(|nested_call| *nested_call == callee)
    }

    pub fn display_str(&self, tcx: TyCtxt<'tcx>) -> String {
        iter::once(tcx.def_path_str(self.root))
            .chain(self.nested_calls.iter().map(|nested_call| format!("{} at {:?}", nested_call.display_str(tcx), tcx.def_span(nested_call.def_id))))
            .intersperse("\n    -> ".to_owned())
            .collect::<String>()
    }
}

pub struct CallGraph<'tcx> {
    pub virtual_calls_count: usize,
    pub dynamic_calls_count: usize,
    pub foreign_calls_count: usize,
    pub root_calls: FxHashMap<hir::LocalDefId, Vec<InstanceCall<'tcx>>>,
    pub nested_calls: Vec<FxHashMap<Callee<'tcx>, Vec<InstanceCall<'tcx>>>>,
}

impl<'tcx> CallGraph<'tcx> {
    pub fn total_calls_count(&self) -> usize {
        let mut total_calls_count = self.root_calls.iter().map(|(_, calls)| calls.len()).sum::<usize>()
            + self.nested_calls.iter().map(|calls| calls.iter().map(|(_, calls)| calls.len()).sum::<usize>()).sum::<usize>();
        // NOTE: Dynamic calls are currently not represented in the call graph, therefore
        //       we have to add their count manually to the total.
        total_calls_count += self.dynamic_calls_count;

        total_calls_count
    }

    pub fn depth(&self) -> usize {
        (!self.root_calls.is_empty() as usize) + self.nested_calls.len()
    }

    pub fn callees_of_nested_caller(&self, caller: Callee<'tcx>) -> Option<&[InstanceCall<'tcx>]> {
        self.nested_calls.iter().find_map(|calls| calls.get(&caller).map(|v| &**v))
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Targeting {
    LocalMutables,
    ExternEntryPoints(hir::CrateNum),
}

impl Targeting {
    pub fn matches<'tcx>(&self, tcx: TyCtxt<'tcx>, test_def_ids: &FxHashSet<hir::LocalDefId>, def_id: hir::DefId) -> Option<TargetKind> {
        match *self {
            Targeting::LocalMutables => {
                let Some(local_def_id) = def_id.as_local() else { return None; };
                let hir_id = tcx.local_def_id_to_hir_id(local_def_id);

                let entry_fn = tcx.entry_fn(());

                // TODO: Ignore #[coverage(off)] functions
                // Functions, excluding closures
                let targeted = matches!(tcx.def_kind(def_id), hir::DefKind::Fn | hir::DefKind::AssocFn)
                    // NOT `fn main() {}`
                    && !entry_fn.map(|(entry_def_id, _)| def_id == entry_def_id).unwrap_or(false)
                    // NOT `const fn`
                    && !tcx.is_const_fn(def_id)
                    // NOT `fn;`
                    && !tcx.hir_node_by_def_id(local_def_id).body_id().is_none()
                    // NOT `#[test]` functions, or inner functions
                    && !test_def_ids.contains(&local_def_id)
                    && !res::parent_iter(tcx, def_id).any(|parent_id| parent_id.as_local().is_some_and(|local_parent_id| test_def_ids.contains(&local_parent_id)))
                    // NOT `#[cfg(test)]` functions, or functions in `#[cfg(test)]` modules
                    && !tests::is_marked_or_in_cfg_test(tcx, hir_id)
                    // NOT `#[mutest::skip]` functions
                    && !tool_attr::skip(tcx.hir_attrs(hir_id));

                if !targeted { return None; }

                Some(TargetKind::LocalMutable(local_def_id))
            }
            Targeting::ExternEntryPoints(cnum) => {
                // NOTE: This is in the context of the local crate invoking the extern crate, so the following can be assumed:
                //       * the definition is visible, otherwise the program would be rejected;
                //       * the definition has a body, as you cannot refer to generic definitions without bodies (e.g. trait assocs)
                //         through monomorphized calls;
                //       * the definition is not a `#[test]` function, a #[cfg(test)] function, or a function in a `#[cfg(test)]` module.

                // NOTE: The `#[mutest::skip]` attribute does not apply here, since that only skips the function
                //       for mutation generation, not for reachability analysis.

                let targeted = def_id.krate == cnum
                    // Functions, excluding closures
                    && matches!(tcx.def_kind(def_id), hir::DefKind::Fn | hir::DefKind::AssocFn);

                if !targeted { return None; }

                Some(TargetKind::ExternEntryPoint(def_id))
            }
        }
    }
}

pub fn instantiate_generic_args<'tcx, T>(tcx: TyCtxt<'tcx>, foldable: T, generic_args: ty::GenericArgsRef<'tcx>) -> T
where
    T: ty::TypeFoldable<TyCtxt<'tcx>>,
{
    ty::EarlyBinder::bind(foldable).instantiate(tcx, generic_args)
}

pub fn reachable_fns<'ast, 'tcx, 'ent>(
    tcx: TyCtxt<'tcx>,
    def_res: &ast_lowering::DefResolutions,
    krate: &'ast ast::Crate,
    entry_points: EntryPoints<'ent>,
    targeting: Targeting,
    depth_limit: Option<usize>,
    trace_length_limit: Option<usize>,
) -> (CallGraph<'tcx>, Vec<Target>) {
    let mut call_graph = CallGraph {
        virtual_calls_count: 0,
        dynamic_calls_count: 0,
        foreign_calls_count: 0,
        root_calls: Default::default(),
        nested_calls: vec![],
    };

    let test_def_ids = match entry_points {
        EntryPoints::Tests(tests) => tests.iter().map(|test| test.def_id).collect::<FxHashSet<_>>(),
        EntryPoints::PublicInterface(_) => Default::default(),
    };

    let mut ignored_entry_points: FxHashSet<EntryPoint> = Default::default();

    let mut previously_found_callees: FxHashSet<Callee<'tcx>> = Default::default();

    for entry_point in entry_points.iter() {
        let body_mir = tcx.instance_mir(ty::InstanceKind::Item(entry_point.def_id.to_def_id()));

        // NOTE: We expect entry points to be non-polymorphic (i.e. no type or const generic) functions.
        //       This is because we cannot build a complete call graph with uninstantiated type and const parameters.
        if body_mir.is_polymorphic {
            match entry_points {
                // Tests cannot be generic functions anyway, so this is a hard crash.
                EntryPoints::Tests(_) => tcx.dcx().fatal("encountered generic function test definition"),
                // NOTE: This does unfortunately rule out tracking generic public interface functions
                //       for cross-crate reachability analysis.
                EntryPoints::PublicInterface(_) => {
                    let mut diagnostic = tcx.dcx().struct_warn("encountered generic function in public interface");
                    diagnostic.span(body_mir.span);
                    diagnostic.note("ignoring generic function: unable to trace all calls accurately due to uninstantiated type and const parameters");
                    diagnostic.emit();

                    ignored_entry_points.insert(entry_point);
                    continue;
                }
            }
        }

        let mut calls = mir_callees(tcx, &body_mir, tcx.mk_args(&[])).collect::<Vec<_>>();
        calls.extend(drop_glue_callees(tcx, &body_mir, tcx.mk_args(&[])));
        // HACK: We must sort the calls into a stable order for the corresponding diagnostics to be printed in a stable order.
        calls.sort_unstable_by(|call_a, call_b| span_diagnostic_ord(call_a.span, call_b.span));

        for call in calls {
            // NOTE: We are post type-checking, querying monomorphic obligations.
            let typing_env = ty::TypingEnv::fully_monomorphized();

            let callee = match call.kind {
                CallKind::Def(def_id, generic_args) => {
                    // Using the concrete type arguments of this call, we resolve the corresponding definition instance. The
                    // type arguments might take a different form at the resolved definition site, so we propagate them
                    // instead.
                    let instance = ty::Instance::expect_resolve(tcx, typing_env, def_id, generic_args, DUMMY_SP);

                    if let ty::InstanceKind::Virtual(def_id, _) = instance.def {
                        call_graph.virtual_calls_count += 1;

                        let mut diagnostic = tcx.dcx().struct_warn("encountered virtual call during call graph construction");
                        diagnostic.span(call.span);
                        diagnostic.span_label(call.span, format!("call to {}", tcx.def_path_str_with_args(def_id, instance.args)));
                        diagnostic.note(format!("in {}", tcx.def_path_str(entry_point.def_id)));
                        diagnostic.emit();
                    }

                    if tcx.is_foreign_item(instance.def_id()) && !tcx.intrinsic(instance.def_id()).is_some() {
                        let codegen_fn_attrs = tcx.codegen_fn_attrs(instance.def_id());
                        let is_allocator_intrinsic = codegen_fn_attrs.flags.intersects(
                            CodegenFnAttrFlags::ALLOCATOR
                            | CodegenFnAttrFlags::DEALLOCATOR
                            | CodegenFnAttrFlags::REALLOCATOR
                            | CodegenFnAttrFlags::ALLOCATOR_ZEROED
                        );

                        if !is_allocator_intrinsic {
                            call_graph.foreign_calls_count += 1;

                            let mut diagnostic = tcx.dcx().struct_warn("encountered foreign call during call graph construction");
                            diagnostic.span(call.span);
                            diagnostic.span_label(call.span, format!("call to {}", tcx.def_path_str_with_args(instance.def_id(), instance.args)));
                            diagnostic.note(format!("in {}", tcx.def_path_str(entry_point.def_id)));
                            diagnostic.emit();
                        }
                    }

                    Callee::new(instance.def_id(), instance.args)
                }

                CallKind::Ptr(fn_sig) => {
                    call_graph.dynamic_calls_count += 1;

                    let mut diagnostic = tcx.dcx().struct_warn("encountered dynamic call during call graph construction");
                    diagnostic.span(call.span);
                    diagnostic.span_label(call.span, format!("call to {fn_sig}"));
                    diagnostic.note(format!("in {}", tcx.def_path_str(entry_point.def_id)));
                    diagnostic.emit();

                    continue;
                }
            };

            let test_calls = call_graph.root_calls.entry(entry_point.def_id).or_default();
            test_calls.push(InstanceCall { callee, safety: call.safety, span: call.span });

            previously_found_callees.insert(callee);
        }
    }

    let mut alread_recorded_callers: FxHashSet<Callee<'tcx>> = Default::default();

    for distance in 0.. {
        let mut newly_found_callees: FxHashSet<Callee<'tcx>> = Default::default();

        // HACK: We must sort the callers into a stable order for the corresponding diagnostics to be printed in a stable order.
        let mut callers = previously_found_callees.drain().collect::<Vec<_>>();
        callers.sort_unstable_by(|caller_a, caller_b| {
            let caller_a_span = tcx.def_span(caller_a.def_id);
            let caller_b_span = tcx.def_span(caller_b.def_id);
            span_diagnostic_ord(caller_a_span, caller_b_span)
        });

        // No remaining callers were found, exit early.
        if callers.is_empty() {
            // Remove the empty entry that was prepared for the nested calls at the last depth.
            if let Some(nested_calls_at_last_depth) = call_graph.nested_calls.pop() {
                assert!(nested_calls_at_last_depth.is_empty(), "no remaining newly found callees to process, but call graph still recorded nested calls");
            };
            break;
        }

        // Reached explicit call graph depth limit.
        if let Some(depth_limit) = depth_limit && !(distance < (depth_limit - 1)) {
            // Warn about non-recorded callers because of explicit call graph depth limit.
            let mut diagnostic = tcx.dcx().struct_warn("incomplete call graph due to explicit depth limit");
            diagnostic.note(format!("call graph depth limit is set to {depth_limit}"));
            diagnostic.note(match callers.len() {
                1 => "ignoring 1 caller and its callees".to_owned(),
                _ => format!("ignoring {} callers and their callees", callers.len()),
            });
            diagnostic.emit();

            break;
        }

        call_graph.nested_calls.push(Default::default());

        let mut callers_to_be_recorded: FxHashSet<Callee<'tcx>> = Default::default();

        for caller in callers {
            // `const` functions, like other `const` scopes, cannot be mutated.
            if tcx.is_const_fn(caller.def_id) { continue; }

            if alread_recorded_callers.contains(&caller) { continue; }

            if !tcx.is_mir_available(caller.def_id) { continue; }
            let body_mir = tcx.instance_mir(ty::InstanceKind::Item(caller.def_id));

            // Collect calls of callees, for the next depth iteration.
            let mut calls = mir_callees(tcx, &body_mir, caller.generic_args).collect::<Vec<_>>();
            calls.extend(drop_glue_callees(tcx, &body_mir, caller.generic_args));
            // HACK: We must sort the calls into a stable order for the corresponding diagnostics to be printed in a stable order.
            calls.sort_unstable_by(|call_a, call_b| span_diagnostic_ord(call_a.span, call_b.span));

            for call in calls {
                // NOTE: We are post type-checking, querying monomorphic obligations.
                let typing_env = ty::TypingEnv::fully_monomorphized();

                let callee = match call.kind {
                    CallKind::Def(def_id, generic_args) => {
                        // The type arguments from the local, generic scope may still contain type parameters, so we
                        // fold the bound type arguments of the concrete invocation of the enclosing function into it.
                        let generic_args = instantiate_generic_args(tcx, generic_args, caller.generic_args);
                        // Using the concrete type arguments of this call, we resolve the corresponding definition
                        // instance. The type arguments might take a different form at the resolved definition site, so
                        // we propagate them instead.
                        let instance = ty::Instance::expect_resolve(tcx, typing_env, def_id, generic_args, DUMMY_SP);

                        if let ty::InstanceKind::Virtual(def_id, _) = instance.def {
                            call_graph.virtual_calls_count += 1;

                            let mut diagnostic = tcx.dcx().struct_warn("encountered virtual call during call graph construction");
                            diagnostic.span(call.span);
                            diagnostic.span_label(call.span, format!("call to {}", tcx.def_path_str_with_args(def_id, instance.args)));
                            diagnostic.note(format!("in {}", tcx.def_path_str_with_args(caller.def_id, caller.generic_args)));
                            diagnostic.emit();
                        }

                        if tcx.is_foreign_item(instance.def_id()) && !tcx.intrinsic(instance.def_id()).is_some() {
                            let codegen_fn_attrs = tcx.codegen_fn_attrs(instance.def_id());
                            let is_allocator_intrinsic = codegen_fn_attrs.flags.intersects(
                                CodegenFnAttrFlags::ALLOCATOR
                                | CodegenFnAttrFlags::DEALLOCATOR
                                | CodegenFnAttrFlags::REALLOCATOR
                                | CodegenFnAttrFlags::ALLOCATOR_ZEROED
                            );

                            if !is_allocator_intrinsic {
                                call_graph.foreign_calls_count += 1;

                                let mut diagnostic = tcx.dcx().struct_warn("encountered foreign call during call graph construction");
                                diagnostic.span(call.span);
                                diagnostic.span_label(call.span, format!("call to {}", tcx.def_path_str_with_args(instance.def_id(), instance.args)));
                                diagnostic.note(format!("in {}", tcx.def_path_str_with_args(caller.def_id, caller.generic_args)));
                                diagnostic.emit();
                            }
                        }

                        Callee::new(instance.def_id(), instance.args)
                    }

                    CallKind::Ptr(fn_sig) => {
                        call_graph.dynamic_calls_count += 1;

                        let mut diagnostic = tcx.dcx().struct_warn("encountered dynamic call during call graph construction");
                        diagnostic.span(call.span);
                        diagnostic.span_label(call.span, format!("call to {fn_sig}"));
                        diagnostic.note(format!("in {}", tcx.def_path_str_with_args(caller.def_id, caller.generic_args)));
                        diagnostic.emit();

                        continue;
                    }
                };

                let caller_calls = call_graph.nested_calls[distance].entry(caller).or_default();
                caller_calls.push(InstanceCall { callee, safety: call.safety, span: call.span });

                newly_found_callees.insert(callee);
            }

            callers_to_be_recorded.insert(caller);
        }

        alread_recorded_callers.extend(callers_to_be_recorded);

        previously_found_callees.extend(newly_found_callees);
    }

    // During the call tree walk along the call traces, for each target, we record
    // each entry point's most severe unsafety source of any of its call paths.
    // Safe items called from an unsafe context (dependencies) will be
    // marked `Unsafety::Tainted` with their corresponding unsafety source.
    //
    // ```ignore
    // [Safe] fn x { [None -> Safe]
    //     [Safe] fn y { [Some(EnclosingUnsafe) -> Unsafe(EnclosingUnsafe)]
    //         unsafe { [Some(Unsafe) -> Unsafe(Unsafe)]
    //             [Safe] fn z { [Some(Unsafe) -> Tainted(Unsafe)] }
    //         }
    //         [Safe] fn w { [Some(EnclosingUnsafe) -> Tainted(EnclosingUnsafe)] }
    //         [Unsafe(Unsafe)] unsafe fn u { [Some(Unsafe) -> Unsafe(Unsafe)]
    //             [Safe] fn v { [Some(Unsafe) -> Tainted(Unsafe)] }
    //             [Safe] fn w { [Some(Unsafe) -> Tainted(Unsafe)] }
    //         }
    //     }
    // }
    // ```

    struct CalleeLookupCache<'tcx, 'a> {
        call_graph: &'a CallGraph<'tcx>,
        cache: UnsafeCell<FxHashMap<Callee<'tcx>, Option<&'a [InstanceCall<'tcx>]>>>,
    }

    impl<'tcx, 'a> CalleeLookupCache<'tcx, 'a> {
        fn new(call_graph: &'a CallGraph<'tcx>) -> Self {
            Self { call_graph, cache: UnsafeCell::new(Default::default()) }
        }

        fn callees_of_nested_caller(&self, caller: Callee<'tcx>) -> &[InstanceCall<'tcx>] {
            // SAFETY: The lookup cache is an append-only map; existing entries are never modified.
            let cache = unsafe { &mut *self.cache.get() };

            let callees = cache.entry(caller).or_insert_with(|| self.call_graph.callees_of_nested_caller(caller));
            callees.unwrap_or_default()
        }
    }

    fn record_nested_targets<'ast, 'tcx>(
        tcx: TyCtxt<'tcx>,
        def_res: &ast_lowering::DefResolutions,
        krate: &'ast ast::Crate,
        test_def_ids: &FxHashSet<hir::LocalDefId>,
        callee_lookup_cache: &CalleeLookupCache<'tcx, '_>,
        entry_point: EntryPoint,
        targeting: Targeting,
        unsafety: Option<UnsafeSource>,
        call_trace: &mut CallTrace<'tcx>,
        targets: &mut FxHashMap<hir::DefId, Target>,
        trace_length_limit: Option<usize>,
    ) {
        let &[.., caller] = &call_trace.nested_calls[..] else { return; };

        let distance = call_trace.nested_calls.len() - 1;

        // `const` functions, like other `const` scopes, cannot be mutated.
        if tcx.is_const_fn(caller.def_id) { return; }

        // Record or update target.
        'target: {
            let target = match targets.entry(caller.def_id) {
                hash_map::Entry::Occupied(entry) => entry.into_mut(),
                hash_map::Entry::Vacant(entry) => {
                    let Some(target_kind) = targeting.matches(tcx, test_def_ids, caller.def_id) else { break 'target; };

                    let item_unsafety = match caller.def_id.as_local() {
                        Some(local_def_id) => {
                            let Some(def_item) = ast_lowering::find_def_in_ast(tcx, def_res, local_def_id, krate) else { break 'target; };
                            check_item_unsafety(def_item)
                        }
                        None => {
                            match tcx.fn_sig(caller.def_id).skip_binder().safety() {
                                hir::Safety::Safe => Unsafety::None,
                                hir::Safety::Unsafe => Unsafety::Unsafe(UnsafeSource::Unsafe),
                            }
                        }
                    };

                    entry.insert(Target {
                        kind: target_kind,
                        unsafety: item_unsafety,
                        reachability: TargetReachability::NestedCallee { distance },
                        reachable_from: Default::default(),
                    })
                }
            };

            if let TargetReachability::NestedCallee { distance: target_distance } = &mut target.reachability {
                *target_distance = Ord::min(distance, *target_distance)
            }

            let caller_tainting = unsafety.map(Unsafety::Tainted).unwrap_or(Unsafety::None);
            target.unsafety = Ord::max(caller_tainting, target.unsafety);

            let entry_point = target.reachable_from.entry(entry_point).or_insert_with(|| {
                EntryPointAssociation {
                    distance,
                    unsafe_call_path: None,
                }
            });

            entry_point.distance = Ord::min(distance, entry_point.distance);
            entry_point.unsafe_call_path = Ord::max(unsafety, entry_point.unsafe_call_path);
        }

        if let Some(trace_length_limit) = trace_length_limit && call_trace.nested_calls.len() >= trace_length_limit {
            tcx.dcx().warn("exceeded explicit call graph trace length limit");
            return;
        }

        for &call in callee_lookup_cache.callees_of_nested_caller(caller) {
            // We have encontered a recursion point along this call trace; end the search along this trace.
            if call_trace.nested_calls.iter().any(|callee_in_trace| call.callee == *callee_in_trace) { continue; }

            call_trace.nested_calls.push(call.callee);

            let unsafety = match call.safety {
                hir::Safety::Safe => unsafety,
                hir::Safety::Unsafe => Some(UnsafeSource::Unsafe),
            };

            record_nested_targets(tcx, def_res, krate, test_def_ids, callee_lookup_cache, entry_point, targeting, unsafety, call_trace, targets, trace_length_limit);

            call_trace.nested_calls.pop();
        }
    }

    let callee_lookup_cache = CalleeLookupCache::new(&call_graph);
    let mut targets: FxHashMap<hir::DefId, Target> = Default::default();
    if let EntryPoints::PublicInterface(def_ids) = entry_points {
        for &def_id in def_ids {
            if ignored_entry_points.contains(&EntryPoint { def_id }) { continue; }

            let Some(target_kind) = targeting.matches(tcx, &test_def_ids, def_id.to_def_id()) else { continue; };

            let Some(def_item) = ast_lowering::find_def_in_ast(tcx, def_res, def_id, krate) else { continue };
            let item_unsafety = check_item_unsafety(def_item);

            targets.insert(def_id.to_def_id(), Target {
                kind: target_kind,
                unsafety: item_unsafety,
                reachability: TargetReachability::DirectEntry,
                reachable_from: Default::default(),
            });
        }
    }
    for (&entry_point, calls) in &call_graph.root_calls {
        let Some(def_item) = ast_lowering::find_def_in_ast(tcx, def_res, entry_point, krate) else { continue };
        let unsafety = check_item_unsafety(def_item);

        for call in calls {
            let unsafety = match unsafety {
                Unsafety::Unsafe(unsafe_source) => Some(unsafe_source),
                _ => None,
            };
            let mut call_trace = CallTrace { root: entry_point, nested_calls: smallvec![call.callee] };
            let entry_point = EntryPoint { def_id: entry_point };
            record_nested_targets(tcx, def_res, krate, &test_def_ids, &callee_lookup_cache, entry_point, targeting, unsafety, &mut call_trace, &mut targets, trace_length_limit);
        }
    }

    (call_graph, targets.into_values().collect())
}
