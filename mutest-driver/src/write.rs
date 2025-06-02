use std::collections::HashMap;
use std::fs;
use std::io::BufWriter;
use std::mem::{self, MaybeUninit};
use std::time::Duration;

use mutest_emit::analysis::call_graph::{CallGraph, Target, Unsafety};
use mutest_emit::analysis::tests::Test;
use mutest_emit::codegen::mutation::{Mutant, MutationConflictGraph, Subst, SubstLoc, UnsafeTargeting};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::LocalDefId;

use crate::config::{self, WriteOptions};
use crate::passes::analysis::AnalysisPassResult;
use crate::passes::compilation::CompilationPassResult;

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

pub fn write_call_graph<'tcx>(write_opts: &WriteOptions, _tcx: TyCtxt<'tcx>, all_mutable_fns_count: usize, call_graph: &CallGraph, reachable_fns: &[Target], duration: Duration) {
    write_metadata(write_opts, "call_graph.json", &mutest_json::call_graph::CallGraphInfo {
        format_version: mutest_json::FORMAT_VERSION,
        stats: mutest_json::call_graph::CallGraphStats {
            all_mutable_fns_count,
            reachable_fns_count: reachable_fns.len(),
            total_calls_count: call_graph.total_calls_count(),
            virtual_calls_count: call_graph.virtual_calls_count,
            dynamic_calls_count: call_graph.dynamic_calls_count,
            foreign_calls_count: call_graph.foreign_calls_count,
        },
        duration,
    });
}

pub fn write_mutations<'tcx>(write_opts: &WriteOptions, tcx: TyCtxt<'tcx>, all_mutable_fns_count: usize, mutants: &[Mutant], unsafe_targeting: UnsafeTargeting, mutation_conflict_graph: &MutationConflictGraph, mutation_batching_algorithm: &config::MutationBatchingAlgorithm, duration: Duration) {
    let total_mutations_count = mutants.iter().map(|mutant| mutant.mutations.len()).sum();

    let mut definitions = mutest_json::IdxVec::new();
    let mut unique_definitions: FxHashMap<LocalDefId, mutest_json::DefId> = Default::default();

    let mut register_def = |def_id: LocalDefId| {
        unique_definitions.entry(def_id).or_insert_with(|| {
            let json_def_id = definitions.next_index();
            definitions.push(mutest_json::Definition {
                def_id: json_def_id,
                name: tcx.opt_item_name(def_id.to_def_id()).map(|symbol| symbol.as_str().to_owned()),
                path: Some(tcx.def_path_str(def_id)),
                span: mutest_json::Span::from_rustc_span(tcx.sess, tcx.def_span(def_id)),
            });
            json_def_id
        });
    };

    for mutant in mutants {
        for mutation in &mutant.mutations {
            register_def(mutation.target.def_id);
        }
    }

    let mutations = {
        let mut mutations = mutest_json::IdxVec::with_capacity(total_mutations_count);
        // NOTE: Mutation IDs are 1-based, so for N mutations, the last mutation's ID will be N.
        mutations.ensure_contains(mutest_json::mutations::MutationId(total_mutations_count as u32), || MaybeUninit::uninit());

        for mutant in mutants {
            for mutation in &mutant.mutations {
                let mutation_id = mutest_json::mutations::MutationId(mutation.id.index());

                let origin_span = mutest_json::Span::from_rustc_span(tcx.sess, mutation.span).expect("invalid span");

                let substs = mutation.substs.iter()
                    .map(|subst| {
                        mutest_json::mutations::Substitution {
                            // TODO: Collect actual substitution spans.
                            location: match &subst.location {
                                SubstLoc::InsertBefore(_) => mutest_json::mutations::SubstitutionLocation::InsertBefore(origin_span.clone()),
                                SubstLoc::InsertAfter(_) => mutest_json::mutations::SubstitutionLocation::InsertAfter(origin_span.clone()),
                                SubstLoc::Replace(_) => mutest_json::mutations::SubstitutionLocation::Replace(origin_span.clone()),
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

                mutations[mutation_id].write(mutest_json::mutations::Mutation {
                    mutation_id,
                    target_def_id: *unique_definitions.get(&mutation.target.def_id).expect("json definitions missing target def id"),
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
        }

        // SAFETY: All mutations have been initialized.
        unsafe { mem::transmute::<_, mutest_json::IdxVec<_, mutest_json::mutations::Mutation>>(mutations) }
    };

    let mutation_batches = match mutation_batching_algorithm {
        config::MutationBatchingAlgorithm::None => None,
        _ => {
            let mut mutation_batches = mutest_json::IdxVec::with_capacity(mutants.len());

            for mutant in mutants {
                let mutation_batch_id = mutation_batches.next_index();
                assert_eq!(mutation_batch_id, mutest_json::mutations::MutationBatchId(mutant.id.index()), "mutants are not supplied in id order");

                mutation_batches.push(mutest_json::mutations::MutationBatch {
                    mutation_batch_id,
                    mutation_ids: mutant.mutations.iter().map(|mutation| mutest_json::mutations::MutationId(mutation.id.index())).collect(),
                });
            }

            Some(mutation_batches)
        },
    };

    let mutated_fns = mutants.iter().flat_map(|mutant| mutant.mutations.iter()).map(|mutation| mutation.target.def_id).collect::<FxHashSet<_>>();
    let mutated_fns_count = mutated_fns.len();

    let mut safe_mutations_count = 0;
    let mut unsafe_mutations_count = 0;
    let mut tainted_mutations_count = 0;
    let mut batched_mutations_count = 0;
    let mut unbatched_mutations_count = 0;
    let mut per_op_stats: HashMap<String, mutest_json::mutations::MutationOpStats> = Default::default();

    for mutant in mutants {
        for mutation in &mutant.mutations {
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

            match &mutant.mutations[..] {
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
        mutations,
        mutation_batches,
        definitions,
        duration,
    });
}

pub fn write_timings(write_opts: &WriteOptions, total_duration: Duration, analysis_pass: &AnalysisPassResult, compilation_pass: Option<&CompilationPassResult>) {
    write_metadata(write_opts, "timings.json", &mutest_json::timings::TimingsInfo {
        total_duration,
        analysis_duration: analysis_pass.duration,
        test_discovery_duration: analysis_pass.test_discovery_duration,
        target_analysis_duration: analysis_pass.target_analysis_duration,
        sanitize_macro_expns_duration: analysis_pass.sanitize_macro_expns_duration,
        mutation_generation_duration: analysis_pass.mutation_generation_duration,
        mutation_conflict_resolution_duration: analysis_pass.mutation_conflict_resolution_duration,
        mutation_batching_duration: analysis_pass.mutation_batching_duration,
        codegen_duration: analysis_pass.codegen_duration,
        write_duration: analysis_pass.write_duration,
        compilation_duration: compilation_pass.map(|compilation_pass| compilation_pass.duration),
    });
}
