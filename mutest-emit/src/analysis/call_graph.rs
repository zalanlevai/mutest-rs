use std::iter;

use rustc_hash::{FxHashSet, FxHashMap};
use rustc_middle::mir;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;

use crate::analysis::ast_lowering;
use crate::analysis::hir;
use crate::analysis::res;
use crate::analysis::tests::{self, Test};
use crate::analysis::ty::{self, TyCtxt};
use crate::codegen::ast;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::mutation::{UnsafeTargeting};
use crate::codegen::symbols::{DUMMY_SP, Span, sym};
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
            | (UnsafeTargeting::OnlyEnclosing(hir::Unsafety::Unsafe), Unsafety::Unsafe(UnsafeSource::EnclosingUnsafe) | Unsafety::Tainted(UnsafeSource::EnclosingUnsafe))
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

    let ast::Unsafe::No = target_fn.sig.header.unsafety else { return Unsafety::Unsafe(UnsafeSource::Unsafe) };

    let Some(target_body) = &target_fn.body else { return Unsafety::None };
    let mut checker = BodyUnsafetyChecker { unsafety: None };
    checker.visit_block(target_body);
    checker.unsafety.unwrap_or(Unsafety::None)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EntryPointAssociation {
    pub distance: usize,
    pub unsafe_call_path: Option<UnsafeSource>,
}

#[derive(Debug)]
pub struct Target<'tst> {
    pub def_id: hir::LocalDefId,
    pub unsafety: Unsafety,
    pub reachable_from: FxHashMap<&'tst Test, EntryPointAssociation>,
    pub distance: usize,
}

impl<'tst> Target<'tst> {
    pub fn is_tainted(&self, entry_point: &Test, unsafe_targeting: UnsafeTargeting) -> bool {
        self.reachable_from.get(entry_point).is_some_and(|entry_point| {
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
                && !tool_attr::skip(tcx.hir().attrs(hir_id))
        })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CallKind<'tcx> {
    Def(hir::DefId, ty::GenericArgsRef<'tcx>),
    Ptr(ty::PolyFnSig<'tcx>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Call<'tcx> {
    pub kind: CallKind<'tcx>,
    pub unsafety: hir::Unsafety,
    pub span: Span,
}

// Based on `rustc_mir_transform::inline::cycle::mir_inliner_callees`.
pub fn mir_callees<'tcx>(tcx: TyCtxt<'tcx>, body_mir: &'tcx mir::Body<'tcx>, generic_args: ty::GenericArgsRef<'tcx>) -> FxHashSet<Call<'tcx>> {
    let instance = ty::Instance { def: body_mir.source.instance, args: generic_args };
    let param_env = ty::ParamEnv::reveal_all();

    body_mir.basic_blocks.iter()
        .filter_map(|basic_block| {
            let terminator = basic_block.terminator();
            let mir::TerminatorKind::Call { func, args: call_args, .. } = &terminator.kind else { return None; };

            let ty = func.ty(&body_mir.local_decls, tcx);
            let span = terminator.source_info.span;

            match ty.kind() {
                &ty::TyKind::FnDef(mut def_id, mut generic_args) => {
                    if tcx.is_intrinsic(def_id, sym::const_eval_select) {
                        let func = &call_args[2].node;
                        let ty = func.ty(&body_mir.local_decls, tcx);
                        let &ty::TyKind::FnDef(inner_def_id, inner_generic_args) = ty.kind() else { return None; };

                        def_id = inner_def_id;
                        generic_args = inner_generic_args;
                    }

                    let generic_args = instance.instantiate_mir_and_normalize_erasing_regions(tcx, param_env, ty::EarlyBinder::bind(generic_args));
                    let unsafety = tcx.fn_sig(def_id).skip_binder().unsafety();
                    Some(Call { kind: CallKind::Def(def_id, generic_args), unsafety, span })
                }

                &ty::TyKind::FnPtr(fn_sig) => {
                    let fn_sig = instance.instantiate_mir_and_normalize_erasing_regions(tcx, param_env, ty::EarlyBinder::bind(fn_sig));
                    let unsafety = fn_sig.unsafety();
                    Some(Call { kind: CallKind::Ptr(fn_sig), unsafety, span })
                }

                _ => None,
            }
        })
        .collect::<FxHashSet<_>>()
}

pub fn drop_glue_callees<'tcx>(tcx: TyCtxt<'tcx>, body_mir: &'tcx mir::Body<'tcx>, generic_args: ty::GenericArgsRef<'tcx>) -> impl Iterator<Item = Call<'tcx>> {
    let instance = ty::Instance { def: body_mir.source.instance, args: generic_args };
    let param_env = ty::ParamEnv::reveal_all();

    body_mir.mentioned_items.iter()
        .filter_map(|mentioned_item| {
            match &mentioned_item.node {
                mir::MentionedItem::Drop(dropped_ty) => Some(dropped_ty),
                _ => None,
            }
        })
        .map(move |&dropped_ty| {
            let dropped_ty = instance.instantiate_mir_and_normalize_erasing_regions(tcx, param_env, ty::EarlyBinder::bind(dropped_ty));
            ty::Instance::resolve_drop_in_place(tcx, dropped_ty)
        })
        .flat_map(move |drop_in_place| tcx.mir_inliner_callees(drop_in_place.def))
        .map(move |&(def_id, generic_args)| {
            let unsafety = tcx.fn_sig(def_id).skip_binder().unsafety();
            Call { kind: CallKind::Def(def_id, generic_args), unsafety, span: DUMMY_SP }
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

pub struct CallGraph<'tcx> {
    pub virtual_calls_count: usize,
    pub dynamic_calls_count: usize,
    pub foreign_calls_count: usize,
    pub root_calls: FxHashSet<(hir::LocalDefId, Callee<'tcx>)>,
    pub nested_calls: Vec<FxHashSet<(Callee<'tcx>, Callee<'tcx>)>>,
}

impl<'tcx> CallGraph<'tcx> {
    pub fn total_calls_count(&self) -> usize {
        let mut total_calls_count = self.root_calls.len() + self.nested_calls.iter().map(|calls| calls.len()).sum::<usize>();
        // NOTE: Dynamic calls are currently not represented in the call graph, therefore
        //       we have to add their count manually to the total.
        total_calls_count += self.dynamic_calls_count;

        total_calls_count
    }
}

pub fn instantiate_generic_args<'tcx, T>(tcx: TyCtxt<'tcx>, foldable: T, generic_args: ty::GenericArgsRef<'tcx>) -> T
where
    T: ty::TypeFoldable<TyCtxt<'tcx>>,
{
    ty::EarlyBinder::bind(foldable).instantiate(tcx, generic_args)
}

pub fn reachable_fns<'ast, 'tcx, 'tst>(
    tcx: TyCtxt<'tcx>,
    def_res: &ast_lowering::DefResolutions,
    krate: &'ast ast::Crate,
    tests: &'tst [Test],
    depth: usize,
) -> (CallGraph<'tcx>, Vec<Target<'tst>>) {
    let mut call_graph = CallGraph {
        virtual_calls_count: 0,
        dynamic_calls_count: 0,
        foreign_calls_count: 0,
        root_calls: Default::default(),
        nested_calls: iter::repeat_with(|| Default::default()).take(depth - 1).collect(),
    };

    /// A map from each entry point to the most severe unsafety source of any call path in its current call tree walk.
    /// Safe items called from an unsafe context (dependencies) will be marked `Unsafety::Tainted` with their
    /// corresponding unsafety source.
    ///
    /// ```ignore
    /// [Safe] fn x { [None -> Safe]
    ///     [Safe] fn y { [Some(EnclosingUnsafe) -> Unsafe(EnclosingUnsafe)]
    ///         unsafe { [Some(Unsafe) -> Unsafe(Unsafe)]
    ///             [Safe] fn z { [Some(Unsafe) -> Tainted(Unsafe)] }
    ///         }
    ///         [Safe] fn w { [Some(EnclosingUnsafe) -> Tainted(EnclosingUnsafe)] }
    ///         [Unsafe(Unsafe)] unsafe fn u { [Some(Unsafe) -> Unsafe(Unsafe)]
    ///             [Safe] fn v { [Some(Unsafe) -> Tainted(Unsafe)] }
    ///             [Safe] fn w { [Some(Unsafe) -> Tainted(Unsafe)] }
    ///         }
    ///     }
    /// }
    /// ```
    type CallPaths<'tst> = FxHashMap<&'tst Test, Option<UnsafeSource>>;

    let test_def_ids = tests.iter().map(|test| test.def_id).collect::<FxHashSet<_>>();

    let mut previously_found_callees: FxHashMap<Callee<'tcx>, CallPaths<'tst>> = Default::default();

    for test in tests {
        if test.ignore { continue; }

        let body_mir = tcx.instance_mir(ty::InstanceDef::Item(test.def_id.to_def_id()));

        let mut callees = mir_callees(tcx, &body_mir, tcx.mk_args(&[]));
        callees.extend(drop_glue_callees(tcx, &body_mir, tcx.mk_args(&[])));

        for call in callees {
            // NOTE: We are post type-checking, querying monomorphic obligations.
            let param_env = ty::ParamEnv::reveal_all();

            let callee = match call.kind {
                CallKind::Def(def_id, generic_args) => {
                    // Using the concrete type arguments of this call, we resolve the corresponding definition instance. The
                    // type arguments might take a different form at the resolved definition site, so we propagate them
                    // instead.
                    let instance = ty::Instance::expect_resolve(tcx, param_env, def_id, generic_args);

                    if let ty::InstanceDef::Virtual(def_id, _) = instance.def {
                        call_graph.virtual_calls_count += 1;

                        let mut diagnostic = tcx.dcx().struct_warn("encountered virtual call during call graph construction");
                        diagnostic.span(call.span);
                        diagnostic.span_label(call.span, format!("call to {}", tcx.def_path_str_with_args(def_id, instance.args)));
                        diagnostic.note(format!("in {}", tcx.def_path_str(test.def_id)));
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
                            diagnostic.note(format!("in {}", tcx.def_path_str(test.def_id)));
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
                    diagnostic.note(format!("in {}", tcx.def_path_str(test.def_id)));
                    diagnostic.emit();

                    continue;
                }
            };

            call_graph.root_calls.insert((test.def_id, callee));

            let call_paths = previously_found_callees.entry(callee).or_insert_with(Default::default);
            call_paths.insert(test, None);
        }
    }

    let mut targets: FxHashMap<hir::LocalDefId, Target> = Default::default();

    for distance in 0..depth {
        let mut newly_found_callees: FxHashMap<Callee<'tcx>, CallPaths<'tst>> = Default::default();

        for (caller, call_paths) in previously_found_callees.drain() {
            // `const` functions, like other `const` scopes, cannot be mutated.
            if tcx.is_const_fn(caller.def_id) { continue; }

            if let Some(local_def_id) = caller.def_id.as_local() {
                if !tcx.hir_node_by_def_id(local_def_id).body_id().is_some() { continue; }

                let hir_id = tcx.local_def_id_to_hir_id(local_def_id);
                let skip = false
                    // Non-functions, including closures
                    || !matches!(tcx.def_kind(caller.def_id), hir::DefKind::Fn | hir::DefKind::AssocFn)
                    // Inner function of #[test] function
                    || res::parent_iter(tcx, caller.def_id).any(|parent_id| parent_id.as_local().is_some_and(|local_parent_id| test_def_ids.contains(&local_parent_id)))
                    // #[cfg(test)] function, or function in #[cfg(test)] module
                    || tests::is_marked_or_in_cfg_test(tcx, hir_id)
                    // #[mutest::skip] function
                    || tool_attr::skip(tcx.hir().attrs(hir_id));

                if !skip && let Some(caller_def_item) = ast_lowering::find_def_in_ast(tcx, def_res, local_def_id, krate) {
                    let target = targets.entry(local_def_id).or_insert_with(|| {
                        Target {
                            def_id: local_def_id,
                            unsafety: check_item_unsafety(caller_def_item),
                            reachable_from: Default::default(),
                            distance,
                        }
                    });

                    for (&test, &unsafety) in &call_paths {
                        let caller_tainting = unsafety.map(Unsafety::Tainted).unwrap_or(Unsafety::None);
                        target.unsafety = Ord::max(caller_tainting, target.unsafety);

                        let entry_point = target.reachable_from.entry(test).or_insert_with(|| {
                            EntryPointAssociation {
                                distance,
                                unsafe_call_path: None,
                            }
                        });

                        entry_point.unsafe_call_path = Ord::max(unsafety, entry_point.unsafe_call_path);
                    }
                }
            }

            // Collect calls of callees, for the next depth iteration.
            // NOTE: This is not performed on the last depth iteration; calls made by
            //       callees at the end of the call graph are ignored.
            if distance < (depth - 1) {
                if !tcx.is_mir_available(caller.def_id) { continue; }
                let body_mir = tcx.instance_mir(ty::InstanceDef::Item(caller.def_id));

                let mut callees = mir_callees(tcx, &body_mir, caller.generic_args);
                callees.extend(drop_glue_callees(tcx, &body_mir, caller.generic_args));

                for call in callees {
                    // NOTE: We are post type-checking, querying monomorphic obligations.
                    let param_env = ty::ParamEnv::reveal_all();

                    let callee = match call.kind {
                        CallKind::Def(def_id, generic_args) => {
                            // The type arguments from the local, generic scope may still contain type parameters, so we
                            // fold the bound type arguments of the concrete invocation of the enclosing function into it.
                            let generic_args = instantiate_generic_args(tcx, generic_args, caller.generic_args);
                            // Using the concrete type arguments of this call, we resolve the corresponding definition
                            // instance. The type arguments might take a different form at the resolved definition site, so
                            // we propagate them instead.
                            let instance = ty::Instance::expect_resolve(tcx, param_env, def_id, generic_args);

                            if let ty::InstanceDef::Virtual(def_id, _) = instance.def {
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

                    call_graph.nested_calls[distance].insert((caller, callee));

                    let new_call_paths = newly_found_callees.entry(callee).or_insert_with(Default::default);

                    for (&test, &unsafety) in &call_paths {
                        let unsafe_source = match call.unsafety {
                            hir::Unsafety::Normal => unsafety,
                            hir::Unsafety::Unsafe => Some(UnsafeSource::Unsafe),
                        };

                        let new_unsafety = new_call_paths.entry(test).or_insert(unsafety);
                        *new_unsafety = new_unsafety.or(unsafe_source);
                    }
                }
            }
        }

        previously_found_callees.extend(newly_found_callees.drain());
    }

    (call_graph, targets.into_values().collect())
}
