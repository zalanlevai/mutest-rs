use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::BufWriter;
use std::time::Duration;

use mutest_emit::analysis::call_graph::{CallGraph, Callee, EntryPoints, Target, TargetKind, TargetReachability, Unsafety};
use mutest_emit::analysis::hir;
use mutest_emit::analysis::tests::Test;
use mutest_emit::codegen::mutation::{Mut, MutationConflictGraph, MutationParallelism, Subst, SubstLoc, UnsafeTargeting};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_middle::bug;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::{DefPathHash, LocalDefId};

use crate::config::WriteOptions;
use crate::passes::analysis::AnalysisPassResult;
use crate::passes::compilation::CompilationPassResult;
use crate::passes::external_mutant::specialized_crate::SpecializedMutantCrateCompilationResult;

fn write_metadata<T: serde::Serialize>(write_opts: &WriteOptions, file_name: &str, data: &T) {
    let file = fs::File::create(write_opts.out_dir.join(file_name)).expect("cannot create metadata file");
    let mut buffered_file = BufWriter::new(file);

    serde_json::to_writer(&mut buffered_file, &data).expect("cannot write metadata file");
}

pub fn write_tests<'tcx>(write_opts: &WriteOptions, tcx: TyCtxt<'tcx>, tests: &[Test], duration: Duration) {
    write_metadata(write_opts, "tests.json", &mutest_json::tests::TestsInfo {
        format_version: mutest_json::FORMAT_VERSION,
        stats: mutest_json::tests::TestStats {
            total_tests_count: tests.len(),
            ignored_tests_count: tests.iter().filter(|test| test.ignore).count(),
        },
        tests: tests.iter()
            .map(|test| {
                mutest_json::tests::Test {
                    name: test.path_str(),
                    span: mutest_json::Span::from_rustc_span(tcx.sess, test.item.span).expect("invalid span"),
                    ignore: test.ignore,
                }
            })
            .collect::<Vec<_>>(),
        duration,
    });
}

pub fn write_call_graph<'tcx, 'ent>(
    write_opts: &WriteOptions,
    tcx: TyCtxt<'tcx>,
    all_mutable_fns_count: usize,
    entry_points: EntryPoints<'ent>,
    call_graph: &CallGraph<'tcx>,
    reachable_fns: &[Target],
    duration: Duration,
) -> FxHashMap<DefPathHash, mutest_json::DefId> {
    if let EntryPoints::External = entry_points {
        bug!("cannot print call graph for external entry points");
    }

    let mut definitions = mutest_json::IdxVec::new();
    let mut unique_definitions: FxHashMap<DefPathHash, mutest_json::DefId> = Default::default();

    macro register_def($def_id:expr) {
        {
            let def_id = $def_id;

            *unique_definitions.entry(tcx.def_path_hash(def_id)).or_insert_with(|| {
                let json_def_id = definitions.next_index();
                definitions.push(mutest_json::Definition {
                    def_id: json_def_id,
                    name: tcx.opt_item_name(def_id).map(|symbol| symbol.as_str().to_owned()),
                    path: Some(tcx.def_path_str(def_id)),
                    span: mutest_json::Span::from_rustc_span(tcx.sess, tcx.def_span(def_id)),
                });

                json_def_id
            })
        }
    }

    // HACK: The later, nested mutations of the collection are not handled well by the compiler,
    //       so we use a `RefCell` instead.
    let callees = RefCell::new(mutest_json::IdxVec::new());
    let mut unique_callees: FxHashMap<Callee<'tcx>, mutest_json::call_graph::CalleeId> = Default::default();

    macro register_callee($callee:expr) {
        {
            let callee = $callee;

            let json_def_id = register_def!(callee.def_id);

            *unique_callees.entry(callee).or_insert_with(|| {
                let json_callee_id = callees.borrow().next_index();
                callees.borrow_mut().push(mutest_json::call_graph::Callee {
                    callee_id: json_callee_id,
                    def_id: json_def_id,
                    generic_args: callee.generic_args.iter().map(|arg| arg.to_string()).collect(),
                    path_with_generic_args: callee.display_str(tcx),
                    calls: Default::default(),
                });

                json_callee_id
            })
        }
    }

    let mut json_entry_points = mutest_json::IdxVec::new();
    for entry_point in entry_points.iter() {
        let _ = register_def!(entry_point.local_def_id.to_def_id());

        let json_entry_point_id = json_entry_points.next_index();
        let mut json_entry_point = mutest_json::call_graph::EntryPoint {
            entry_point_id: json_entry_point_id,
            name: tcx.opt_item_name(entry_point.local_def_id.to_def_id()).map(|symbol| symbol.as_str().to_owned()).unwrap_or_default(),
            path: tcx.def_path_str(entry_point.local_def_id.to_def_id()),
            span: mutest_json::Span::from_rustc_span(tcx.sess, tcx.def_span(entry_point.local_def_id.to_def_id())),
            calls: Default::default(),
        };

        for call in call_graph.root_calls.get(&entry_point.local_def_id).into_iter().flatten() {
            let json_callee_id = register_callee!(call.callee);

            let call_instances = json_entry_point.calls.entry(json_callee_id).or_default();
            call_instances.push(mutest_json::call_graph::CallInstance {
                span: mutest_json::Span::from_rustc_span(tcx.sess, call.span),
                safety: match call.safety {
                    hir::Safety::Safe => mutest_json::Safety::Safe,
                    hir::Safety::Unsafe => mutest_json::Safety::Unsafe,
                },
            });
        }

        json_entry_points.push(json_entry_point);
    }

    for nested_calls in &call_graph.nested_calls {
        for (caller, calls) in nested_calls {
            let json_callee_id = register_callee!(*caller);
            // HACK: The mutations extending `callees` in the nested loop may
            //       invalidate the mutable reference to this entry,
            //       so we make a copy, and write the updates back afterwards.
            let mut callee_calls = callees.borrow()[json_callee_id].calls.clone();

            for call in calls {
                let json_callee_id = register_callee!(call.callee);

                let call_instances = callee_calls.entry(json_callee_id).or_default();
                call_instances.push(mutest_json::call_graph::CallInstance {
                    span: mutest_json::Span::from_rustc_span(tcx.sess, call.span),
                    safety: match call.safety {
                        hir::Safety::Safe => mutest_json::Safety::Safe,
                        hir::Safety::Unsafe => mutest_json::Safety::Unsafe,
                    },
                });
            }

            callees.borrow_mut()[json_callee_id].calls = callee_calls;
        }
    }

    write_metadata(write_opts, "call_graph.json", &mutest_json::call_graph::CallGraphInfo {
        format_version: mutest_json::FORMAT_VERSION,
        stats: mutest_json::call_graph::CallGraphStats {
            all_mutable_fns_count,
            reachable_fns_count: reachable_fns.len(),
            total_calls_count: call_graph.total_calls_count(),
            virtual_calls_count: call_graph.virtual_calls_count,
            dynamic_calls_count: call_graph.dynamic_calls_count,
            foreign_calls_count: call_graph.foreign_calls_count,
            call_graph_depth: call_graph.depth(),
        },
        call_graph: mutest_json::call_graph::CallGraph {
            entry_points_kind: match entry_points {
                EntryPoints::Tests(_) => mutest_json::call_graph::EntryPointsKind::Tests,
                EntryPoints::External => unreachable!(),
            },
            entry_points: json_entry_points,
            callees: callees.take(),
        },
        definitions,
        duration,
    });

    unique_definitions
}

pub fn write_mutations<'tcx, 'trg>(
    write_opts: &WriteOptions,
    tcx: TyCtxt<'tcx>,
    all_mutable_fns_count: usize,
    json_definitions: &FxHashMap<DefPathHash, mutest_json::DefId>,
    targets: &'trg [Target],
    mutations: &[Mut],
    unsafe_targeting: UnsafeTargeting,
    mutation_conflict_graph: &MutationConflictGraph,
    mutation_parallelism: Option<MutationParallelism>,
    duration: Duration,
) {
    let total_mutations_count = mutations.len();

    let mut json_targets = mutest_json::IdxVec::new();
    let mut target_id_allocation: FxHashMap<LocalDefId, mutest_json::mutations::TargetId> = Default::default();
    for target in targets {
        let TargetKind::LocalMutable(local_def_id) = target.kind else {
            bug!("encountered mutation with non-local mutation target while writing mutations");
        };

        let json_target_id = json_targets.next_index();
        json_targets.push(mutest_json::mutations::Target {
            target_id: json_target_id,
            def_id: *json_definitions.get(&tcx.def_path_hash(target.def_id())).expect("json definitions missing target def id"),
            safety: match target.unsafety {
                Unsafety::Unsafe(_) => mutest_json::mutations::MutationSafety::Unsafe,
                Unsafety::Tainted(_) => mutest_json::mutations::MutationSafety::Tainted,
                Unsafety::None => mutest_json::mutations::MutationSafety::Safe,
            },
            reachability: match target.reachability {
                TargetReachability::DirectEntry => mutest_json::mutations::TargetReachability::DirectEntryPoint,
                TargetReachability::NestedCallee { distance } => mutest_json::mutations::TargetReachability::NestedCallee { distance },
            },
            reachable_from: target.reachable_from.iter()
                .map(|(entry_point, entry_point_assoc)| {
                    (entry_point.path_str(tcx), mutest_json::mutations::EntryPointAssociation {
                        distance: entry_point_assoc.distance,
                        tainted_call_path: target.is_tainted(entry_point, unsafe_targeting),
                    })
                })
                .collect(),
        });

        target_id_allocation.insert(local_def_id, json_target_id);
    }

    let mut json_mutations = mutest_json::IdxVec::with_capacity(total_mutations_count);
    for mutation in mutations {
        let TargetKind::LocalMutable(local_def_id) = mutation.target.kind else {
            bug!("encountered mutation with non-local mutation target while writing mutations");
        };

        let mutation_id = json_mutations.next_index();
        assert_eq!(mutation_id, mutest_json::mutations::MutationId(mutation.id.index()), "mutations are not supplied in id order");

        let origin_span = mutest_json::Span::from_rustc_span(tcx.sess, mutation.span).expect("invalid span");

        let substs = mutation.substs.iter()
            .map(|subst| {
                mutest_json::mutations::Substitution {
                    location: match &subst.location {
                        SubstLoc::InsertBefore(_, span) => {
                            let subst_span = mutest_json::Span::from_rustc_span(tcx.sess, *span).expect("invalid span");
                            mutest_json::mutations::SubstitutionLocation::InsertBefore(subst_span)
                        }
                        SubstLoc::InsertAfter(_, span) => {
                            let subst_span = mutest_json::Span::from_rustc_span(tcx.sess, *span).expect("invalid span");
                            mutest_json::mutations::SubstitutionLocation::InsertAfter(subst_span)
                        }
                        SubstLoc::Replace(_, span) => {
                            let subst_span = mutest_json::Span::from_rustc_span(tcx.sess, *span).expect("invalid span");
                            mutest_json::mutations::SubstitutionLocation::Replace(subst_span)
                        }
                    },
                    substitute: mutest_json::mutations::Substitute {
                        kind: match &subst.substitute {
                            Subst::AstExpr(..) => mutest_json::mutations::SubstituteKind::Expr,
                            Subst::AstStmt(..) => mutest_json::mutations::SubstituteKind::Stmt,
                            Subst::AstLocal(..) => mutest_json::mutations::SubstituteKind::Local,
                        },
                        replacement: subst.substitute.to_source_string(),
                    },
                }
            })
            .collect();

        json_mutations.push(mutest_json::mutations::Mutation {
            mutation_id,
            target_id: *target_id_allocation.get(&local_def_id).expect("target def id not allocated"),
            origin_span,
            mutation_op: mutation.op_name().to_owned(),
            display_name: mutation.display_name(),
            substs,
            safety: match (mutation.is_unsafe(unsafe_targeting), mutation.target.unsafety) {
                (true, Unsafety::Tainted(_)) => mutest_json::mutations::MutationSafety::Tainted,
                (true, _) => mutest_json::mutations::MutationSafety::Unsafe,
                (false, _) => mutest_json::mutations::MutationSafety::Safe,
            },
        });
    }

    let mutation_batches = match mutation_parallelism {
        Some(MutationParallelism::Batched(mutation_batches)) => {
            let mut json_mutation_batches = mutest_json::IdxVec::with_capacity(mutation_batches.len());

            for mutation_batch in mutation_batches {
                let mutation_batch_id = json_mutation_batches.next_index();
                assert_eq!(mutation_batch_id, mutest_json::mutations::MutationBatchId(mutation_batch.id.index()), "mutation batches are not supplied in id order");

                json_mutation_batches.push(mutest_json::mutations::MutationBatch {
                    mutation_batch_id,
                    mutation_ids: mutation_batch.mutations.iter().map(|mutation| mutest_json::mutations::MutationId(mutation.id.index())).collect(),
                });
            }

            Some(json_mutation_batches)
        }
        _ => None,
    };

    let mutated_fns = mutations.iter().map(|mutation| mutation.target.def_id()).collect::<FxHashSet<_>>();
    let mutated_fns_count = mutated_fns.len();

    let mut safe_mutations_count = 0;
    let mut unsafe_mutations_count = 0;
    let mut tainted_mutations_count = 0;
    let mut batched_mutations_count = 0;
    let mut unbatched_mutations_count = 0;
    let mut per_op_stats: HashMap<String, mutest_json::mutations::MutationOpStats> = Default::default();

    for mutation in mutations {
        let op_stats = per_op_stats.entry(mutation.op_name().to_owned()).or_default();

        op_stats.total_mutations_count += 1;

        let is_unsafe_according_to_targeting = mutation.is_unsafe(unsafe_targeting);
        let is_target_tainted = matches!(mutation.target.unsafety, Unsafety::Tainted(_));

        if is_unsafe_according_to_targeting {
            unsafe_mutations_count += 1;
            op_stats.unsafe_mutations_count += 1;
        }
        if is_target_tainted {
            tainted_mutations_count += 1;
            op_stats.tainted_mutations_count +=1;
        }

        if !is_unsafe_according_to_targeting && !is_target_tainted {
            safe_mutations_count += 1;
            op_stats.safe_mutations_count += 1;
        }
    }

    match mutation_parallelism {
        Some(MutationParallelism::Batched(mutation_batches)) => {
            for mutation_batch in mutation_batches {
                for mutation in &mutation_batch.mutations {
                    let Some(op_stats) = per_op_stats.get_mut(mutation.op_name()) else { unreachable!() };

                    match &mutation_batch.mutations[..] {
                        [_] => {
                            unbatched_mutations_count += 1;
                            op_stats.unbatched_mutations_count += 1;
                        }
                        _ => {
                            batched_mutations_count += 1;
                            op_stats.batched_mutations_count += 1;
                        }
                    }
                }
            }
        }
        _ => {
            unbatched_mutations_count = total_mutations_count;
            for (_, op_stat) in &mut per_op_stats {
                op_stat.unbatched_mutations_count = op_stat.total_mutations_count;
            }
        }
    }

    write_metadata(write_opts, "mutations.json", &mutest_json::mutations::MutationsInfo {
        format_version: mutest_json::FORMAT_VERSION,
        stats: mutest_json::mutations::MutationStats {
            all_mutable_fns_count,
            mutated_fns_count,
            total_mutations_count,
            safe_mutations_count,
            unsafe_mutations_count,
            tainted_mutations_count,
            mutation_conflicts_count: mutation_conflict_graph.iter_conflicts().count(),
            mutation_conflicts_count_excluding_unsafe: mutation_conflict_graph.iter_conflicts_excluding_unsafe().count(),
            mutation_compatibilities_count: mutation_conflict_graph.iter_compatibilities().count(),
            batched_mutations_count,
            unbatched_mutations_count,
        },
        per_op_stats,
        mutations: json_mutations,
        mutation_batches,
        targets: json_targets,
        duration,
    });
}

pub fn write_timings(
    write_opts: &WriteOptions,
    total_duration: Duration,
    analysis_pass: &AnalysisPassResult,
    specialized_external_mutant_pass: Option<&SpecializedMutantCrateCompilationResult>,
    compilation_pass: Option<&CompilationPassResult>,
) {
    let mut analysis_duration = analysis_pass.duration;
    if let Some(p) = specialized_external_mutant_pass && let Some(p) = &p.nested_run_result.analysis_pass {
        analysis_duration += p.duration;
    }

    let mut sanitize_macro_expns_duration = analysis_pass.sanitize_macro_expns_duration;
    if let Some(p) = specialized_external_mutant_pass && let Some(p) = &p.nested_run_result.analysis_pass {
        sanitize_macro_expns_duration += p.sanitize_macro_expns_duration;
    }

    let mutation_generation_duration = match specialized_external_mutant_pass {
        Some(p) if let Some(p) = &p.nested_run_result.analysis_pass => p.mutation_generation_duration,
        _ => analysis_pass.mutation_generation_duration,
    };

    let mutation_conflict_resolution_duration = match specialized_external_mutant_pass {
        Some(p) if let Some(p) = &p.nested_run_result.analysis_pass => p.mutation_conflict_resolution_duration,
        _ => analysis_pass.mutation_conflict_resolution_duration,
    };

    let mutation_batching_duration = match specialized_external_mutant_pass {
        Some(p) if let Some(p) = &p.nested_run_result.analysis_pass => p.mutation_batching_duration,
        _ => analysis_pass.mutation_batching_duration,
    };

    let mut codegen_duration = analysis_pass.codegen_duration;
    if let Some(p) = specialized_external_mutant_pass && let Some(p) = &p.nested_run_result.analysis_pass {
        codegen_duration += p.codegen_duration;
    }

    let mut write_duration = analysis_pass.write_duration;
    if let Some(p) = specialized_external_mutant_pass && let Some(p) = &p.nested_run_result.analysis_pass {
        write_duration += p.write_duration;
    }

    let external_tests_compilation_duration = match specialized_external_mutant_pass {
        Some(_) => compilation_pass.map(|p| p.duration),
        _ => None,
    };

    let mutant_compilation_duration = match specialized_external_mutant_pass {
        Some(p) => p.nested_run_result.compilation_pass.as_ref().map(|p| p.duration),
        _ => compilation_pass.map(|p| p.duration),
    };

    let total_compilation_duration = compilation_pass.map(|p| {
        let mut total_compilation_duration = p.duration;
        if let Some(p) = specialized_external_mutant_pass && let Some(p) = &p.nested_run_result.compilation_pass {
            total_compilation_duration += p.duration;
        }
        total_compilation_duration
    });

    write_metadata(write_opts, "timings.json", &mutest_json::timings::TimingsInfo {
        total_duration,
        analysis_duration,
        test_discovery_duration: analysis_pass.test_discovery_duration,
        target_analysis_duration: analysis_pass.target_analysis_duration,
        sanitize_macro_expns_duration,
        mutation_generation_duration,
        mutation_conflict_resolution_duration,
        mutation_batching_duration,
        codegen_duration,
        write_duration,
        external_tests_compilation_duration,
        mutant_compilation_duration,
        total_compilation_duration,
    });
}
