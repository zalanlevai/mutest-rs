use std::iter;

use mutest_emit::analysis::call_graph::{CallGraph, Callee, Target, Unsafety};
use mutest_emit::analysis::tests::Test;
use mutest_emit::codegen::symbols::span_diagnostic_ord;
use mutest_emit::codegen::mutation::{Mut, MutId, Mutant, MutationConflictGraph, UnsafeTargeting};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;
use smallvec::{SmallVec, smallvec};

use crate::config;

pub fn print_tests(tests: &[Test]) {
    let mut ignored_tests_count = 0;

    let mut tests_in_print_order = tests.iter()
        .map(|test| (test.path_str(), test))
        .collect::<Vec<_>>();
    tests_in_print_order.sort_unstable_by(|(test_a_path_str, _), (test_b_path_str, _)| Ord::cmp(test_a_path_str, test_b_path_str));

    let tests_count = tests_in_print_order.len();

    for (test_path_str, test) in tests_in_print_order {
        let mut marker = "";
        if test.ignore {
            marker = " [ignored]";
            ignored_tests_count += 1;
        }

        println!("test {test}{marker}",
            test = test_path_str,
        );
    }
    println!();

    println!("tests: {total} total; {ignored} ignored",
        total = tests_count,
        ignored = ignored_tests_count,
    );
}

pub fn print_targets<'tcx, 'trg>(tcx: TyCtxt<'tcx>, targets: impl Iterator<Item = &'trg Target<'trg>>, unsafe_targeting: UnsafeTargeting) {
    let mut unsafe_targets_count = 0;
    let mut tainted_targets_count = 0;

    // Targets are printed in source span order.
    let mut targets_in_print_order = targets
        .map(|target| (tcx.hir_span(tcx.local_def_id_to_hir_id(target.def_id)), target))
        .collect::<Vec<_>>();
    targets_in_print_order.sort_unstable_by(|(target_a_span, _), (target_b_span, _)| span_diagnostic_ord(*target_a_span, *target_b_span));

    let targets_count = targets_in_print_order.len();

    for (target_span, target) in targets_in_print_order {
        let mut unsafe_marker = "";
        match (target.unsafety.is_unsafe(unsafe_targeting), target.unsafety) {
            (true, Unsafety::Tainted(_)) => {
                unsafe_targets_count += 1;
                tainted_targets_count += 1;
                unsafe_marker = "[tainted] ";
            }
            (true, _) => {
                unsafe_targets_count += 1;
                unsafe_marker = "[unsafe] ";
            }
            (false, _) => {}
        };

        println!("tests -({distance})-> {unsafe_marker}{def_path} at {span:#?}",
            distance = target.distance,
            def_path = tcx.def_path_str(target.def_id.to_def_id()),
            span = target_span,
        );

        // Entry points are printed in order of distance first, within that by lexical order of their definition path.
        let mut entry_points_in_print_order = target.reachable_from.iter()
            .map(|(&test, entry_point)| (test.path_str(), test, entry_point))
            .collect::<Vec<_>>();
        entry_points_in_print_order.sort_unstable_by(|(test_a_path_str, _, entry_point_a), (test_b_path_str, _, entry_point_b)| {
            Ord::cmp(&entry_point_a.distance, &entry_point_b.distance).then(Ord::cmp(test_a_path_str, test_b_path_str))
        });

        for (test_path_str, test, entry_point) in entry_points_in_print_order {
            println!("  ({distance}) {tainted_marker}{test}",
                distance = entry_point.distance,
                tainted_marker = match target.is_tainted(test, unsafe_targeting) {
                    true => "[tainted] ",
                    false => "",
                },
                test = test_path_str,
            );
        }

        println!();
    }

    println!("targets: {total} total; {safe} safe; {unsafe} unsafe ({tainted} tainted)",
        total = targets_count,
        safe = targets_count - unsafe_targets_count,
        r#unsafe = unsafe_targets_count,
        tainted = tainted_targets_count,
    );
}

pub fn print_call_graph<'tcx, 'trg>(tcx: TyCtxt<'tcx>, tests: &[Test], call_graph: &CallGraph<'tcx>, targets: &[Target<'trg>], format: config::GraphFormat, test_filters: &[String], non_local_call_view: config::CallGraphNonLocalCallView) {
    let mut filtered_nodes: FxHashSet<Callee> = Default::default();

    match format {
        config::GraphFormat::Simple => {
            let mut tests_in_print_order = tests.iter()
                .map(|test| (test.path_str(), test))
                .collect::<Vec<_>>();
            tests_in_print_order.sort_unstable_by(|(test_a_path_str, _), (test_b_path_str, _)| Ord::cmp(test_a_path_str, test_b_path_str));

            println!("entry points:\n");

            for (test_path_str, test) in tests_in_print_order {
                if !test_filters.is_empty() {
                    if !test_filters.iter().any(|test_filter| test_path_str.contains(test_filter)) { continue; }
                    filtered_nodes.insert(Callee::new(test.def_id.to_def_id(), tcx.mk_args(&[])));
                }

                println!("test {}", test_path_str);

                let mut callees_in_print_order = call_graph.root_calls.iter()
                    .filter(|(root_def_id, _, _)| *root_def_id == test.def_id)
                    .map(|(_, callee, _)| (callee.display_str(tcx), tcx.def_span(callee.def_id), callee))
                    .collect::<Vec<_>>();
                callees_in_print_order.sort_unstable_by(|(callee_a_display_str, callee_a_span, _), (callee_b_display_str, callee_b_span, _)| {
                    span_diagnostic_ord(*callee_a_span, *callee_b_span).then(Ord::cmp(callee_a_display_str, callee_b_display_str))
                });

                for (callee_display_str, callee_span, callee) in callees_in_print_order {
                    if !test_filters.is_empty() {
                        filtered_nodes.insert(*callee);
                    }

                    println!("  -> {} at {:#?}", callee_display_str, callee_span);
                }
            }

            for (distance, calls) in iter::zip(1.., &call_graph.nested_calls) {
                println!("\nnested calls at distance {distance}:\n");

                let callers = calls.iter()
                    .map(|(caller, _, _)| caller)
                    .filter(|caller| test_filters.is_empty() || filtered_nodes.contains(caller))
                    .collect::<FxHashSet<_>>();
                let mut callers_in_print_order = callers.into_iter()
                    .map(|caller| (caller.display_str(tcx), tcx.def_span(caller.def_id), caller))
                    .collect::<Vec<_>>();
                callers_in_print_order.sort_unstable_by(|(caller_a_display_str, caller_a_span, _), (caller_b_display_str, caller_b_span, _)| {
                    span_diagnostic_ord(*caller_a_span, *caller_b_span).then(Ord::cmp(caller_a_display_str, caller_b_display_str))
                });

                for (caller_display_str, caller_span, caller) in callers_in_print_order {
                    println!("{} at {:#?}", caller_display_str, caller_span);

                    let mut callees_in_print_order = calls.iter()
                        .filter(|(this_caller, _, _)| this_caller == caller)
                        .map(|(_, callee, _)| (callee.display_str(tcx), tcx.def_span(callee.def_id), callee))
                        .collect::<Vec<_>>();
                    callees_in_print_order.sort_unstable_by(|(callee_a_display_str, callee_a_span, _), (callee_b_display_str, callee_b_span, _)| {
                        span_diagnostic_ord(*callee_a_span, *callee_b_span).then(Ord::cmp(callee_a_display_str, callee_b_display_str))
                    });

                    for (callee_display_str, callee_span, callee) in callees_in_print_order {
                        if !test_filters.is_empty() {
                            filtered_nodes.insert(*callee);
                        }

                        println!("  -> {} at {:#?}", callee_display_str, callee_span);
                    }
                }
            }
        }
        config::GraphFormat::Graphviz => {
            let def_node_id = |def_id: DefId| format!("def_{}_{}", def_id.krate.index(), def_id.index.index());

            let callee_node_id = |callee: &Callee| {
                if callee.generic_args.is_empty() { return def_node_id(callee.def_id); }
                format!("def_{}_{}_{}", callee.def_id.krate.index(), callee.def_id.index.index(), &format!("{:p}", callee.generic_args)[2..])
            };

            let escape_label_str = |label: &str| rustc_graphviz::LabelText::LabelStr(std::borrow::Cow::Borrowed(label)).to_dot_string();

            println!("strict digraph {{");
            println!("  layout=dot;");
            println!("  rankdir=LR;");
            println!("  concentrate=true;");
            println!("  ranksep=1;");
            println!("  forcelabels=true;");

            // Distance markers. Also used as "anchors" for the depth-based ranks.
            println!("  node [shape=none];");
            println!("  _ [label=\"distance:\"];");
            print!("  _");
            for distance in 0..=call_graph.nested_calls.len() { print!(" -> {distance}"); }
            println!(" [arrowhead=none];");

            println!("  node [shape=ellipse];");

            println!("  subgraph cluster_tests {{");
            println!("    label=\"tests (entry points)\";");
            println!("    rank=same");
            println!("    style=filled;");
            println!("    color=lightgray;");
            println!("    node [style=filled, color=white];");

            for test in tests {
                let test_path_str = test.path_str();

                if !test_filters.is_empty() {
                    if !test_filters.iter().any(|test_filter| test_path_str.contains(test_filter)) { continue; }
                    filtered_nodes.insert(Callee::new(test.def_id.to_def_id(), tcx.mk_args(&[])));
                }

                // TODO: Use different styling for ignored test, or use strikethrough.
                println!("    {} [label=\"{}\"];", def_node_id(test.def_id.to_def_id()), test_path_str);
            }

            println!("  }}");

            let mut defined_callees: FxHashSet<Callee> = Default::default();
            let mut collapsed_call_paths: FxHashMap::<Callee<'tcx>, SmallVec<[SmallVec<[Callee<'tcx>; 3]>; 1]>> = Default::default();

            println!("  {{ rank=same; 0;");
            let unique_root_callees = call_graph.root_calls.iter()
                .filter(|(root_def_id, _, _)| test_filters.is_empty() || filtered_nodes.contains(&Callee::new(root_def_id.to_def_id(), tcx.mk_args(&[]))))
                .map(|(_, callee, _)| callee)
                .collect::<FxHashSet<_>>();
            for callee in unique_root_callees {
                if !defined_callees.insert(*callee) { continue; }

                // Override non-local callee node rendering.
                if !callee.def_id.is_local() {
                    match non_local_call_view {
                        config::CallGraphNonLocalCallView::Collapse => {}
                        config::CallGraphNonLocalCallView::Expand => {
                            println!("    {} [label={}, shape=plaintext, fontcolor=lightgray];", callee_node_id(callee), escape_label_str(&callee.display_str(tcx)));
                        }
                    }
                    continue;
                }

                match targets.iter().any(|target| target.def_id.to_def_id() == callee.def_id) {
                    true => println!("    {} [label={}];", callee_node_id(callee), escape_label_str(&callee.display_str(tcx))),
                    false => println!("    {} [label={}, shape=plaintext];", callee_node_id(callee), escape_label_str(&callee.display_str(tcx))),
                }
            }
            println!("  }}");
            for (root_def_id, callee, _) in &call_graph.root_calls {
                if !test_filters.is_empty() {
                    if !filtered_nodes.contains(&Callee::new(root_def_id.to_def_id(), tcx.mk_args(&[]))) { continue; }
                    filtered_nodes.insert(*callee);
                }

                // Override non-local root call edge rendering.
                if !callee.def_id.is_local() {
                    match non_local_call_view {
                        config::CallGraphNonLocalCallView::Collapse => {
                            collapsed_call_paths.entry(*callee).or_default().push(smallvec![Callee::new(root_def_id.to_def_id(), tcx.mk_args(&[])), *callee]);
                        }
                        config::CallGraphNonLocalCallView::Expand => {
                            println!("  {} -> {}:w [color=lightgray];", def_node_id(root_def_id.to_def_id()), callee_node_id(callee));
                        }
                    }
                    continue;
                }

                println!("  {} -> {}:w;", def_node_id(root_def_id.to_def_id()), callee_node_id(callee));
            }

            for (distance, calls) in iter::zip(1.., &call_graph.nested_calls) {
                println!("  {{ rank=same; {};", distance);
                let unique_nested_callees = calls.iter()
                    .filter(|(caller, _, _)| test_filters.is_empty() || filtered_nodes.contains(caller))
                    .map(|(_, callee, _)| callee)
                    .collect::<FxHashSet<_>>();
                for callee in unique_nested_callees {
                    if !defined_callees.insert(*callee) { continue; }

                    // Override non-local callee node rendering.
                    if !callee.def_id.is_local() {
                        match non_local_call_view {
                            config::CallGraphNonLocalCallView::Collapse => {}
                            config::CallGraphNonLocalCallView::Expand => {
                                println!("    {} [label={}, shape=plaintext, fontcolor=lightgray];", callee_node_id(callee), escape_label_str(&callee.display_str(tcx)));
                            }
                        }
                        continue;
                    }

                    match targets.iter().any(|target| target.def_id.to_def_id() == callee.def_id) {
                        true => println!("    {} [label={}];", callee_node_id(callee), escape_label_str(&callee.display_str(tcx))),
                        false => println!("    {} [label={}, shape=plaintext];", callee_node_id(callee), escape_label_str(&callee.display_str(tcx))),
                    }
                }
                println!("  }}");
                for (caller, callee, _) in calls {
                    if !test_filters.is_empty() {
                        if !filtered_nodes.contains(caller) { continue; }
                        filtered_nodes.insert(*callee);
                    }

                    // Override non-local deep call edge rendering.
                    if !caller.def_id.is_local() || !callee.def_id.is_local() {
                        match non_local_call_view {
                            config::CallGraphNonLocalCallView::Collapse => {
                                match (caller.def_id.is_local(), callee.def_id.is_local()) {
                                    (true, true) => unreachable!(),
                                    // Local definition calls into non-local definition.
                                    (true, false) => {
                                        // Record new collapsed non-local call path.
                                        collapsed_call_paths.entry(*callee).or_default().push(smallvec![*caller, *callee]);
                                    }
                                    // Non-local definition calls into other non-local definition.
                                    (false, false) => {
                                        // Extend collapsed non-local call paths.
                                        let Some(call_paths) = collapsed_call_paths.get(caller) else { unreachable!() };
                                        let mut call_paths = call_paths.clone();
                                        for call_path in &mut call_paths {
                                            call_path.push(*callee);
                                        }
                                        collapsed_call_paths.entry(*callee).or_default().extend(call_paths);
                                    }
                                    // Non-local definition calls into local definition.
                                    (false, true) => {
                                        // Write out collapsed non-local call paths.
                                        let Some(call_paths) = collapsed_call_paths.get(caller) else { unreachable!(); };
                                        for call_path in call_paths {
                                            let [root_callee, ..] = &call_path[..] else { unreachable!() };
                                            let tooltip = call_path.iter().chain(iter::once(callee))
                                                .map(|callee| callee.display_str(tcx))
                                                .intersperse("\n-> ".to_owned())
                                                .collect::<String>();
                                            println!("  {} -> {}:w [label=\"indirect\", color=lightgray, labeltooltip={}];", callee_node_id(root_callee), callee_node_id(callee), escape_label_str(&tooltip));
                                        }
                                    }
                                }
                            }
                            config::CallGraphNonLocalCallView::Expand => {
                                println!("  {} -> {}:w [color=lightgray];", callee_node_id(caller), callee_node_id(callee));
                            }
                        }
                        continue;
                    }

                    println!("  {} -> {}:w;", callee_node_id(caller), callee_node_id(callee));
                }
            }

            println!("}}");
        }
    }
}

pub fn print_mutation_graph<'trg: 'op, 'm: 'op, 'op, N, E>(mutation_conflict_graph: &MutationConflictGraph<'m>, mutations_iter: N, edge_iter: E, format: config::GraphFormat)
where
    N: IntoIterator<Item = &'op Mut<'trg, 'm>>,
    E: IntoIterator<Item = (MutId, MutId)>,
{
    match format {
        config::GraphFormat::Simple => {
            for (a, b) in edge_iter {
                println!("{} {}", a.index(), b.index());
            }
        }
        config::GraphFormat::Graphviz => {
            println!("strict graph {{");
            println!("  overlap=\"false\";");
            println!("  splines=\"true\";");

            for m in mutations_iter.into_iter() {
                let mut attrs = vec![];
                if mutation_conflict_graph.is_unsafe(m.id) {
                    attrs.push("color=\"red\"");
                    attrs.push("fontcolor=\"red\"");
                }

                println!("  {}{attrs};", m.id.index(),
                    attrs = match attrs.is_empty() {
                        true => "".to_owned(),
                        false => format!(" [{}]", attrs.join(", ")),
                    },
                );
            }

            for (a, b) in edge_iter.into_iter() {
                println!("  {} -- {};", a.index(), b.index());
            }

            println!("}}");
        }
    }
}

pub fn print_mutants<'tcx>(tcx: TyCtxt<'tcx>, mutants: &[Mutant], unsafe_targeting: UnsafeTargeting, verbosity: u8) {
    let mut total_mutations_count = 0;
    let mut unsafe_mutations_count = 0;
    let mut tainted_mutations_count = 0;
    let mut unbatched_mutations_count = 0;

    for mutant in mutants {
        total_mutations_count += mutant.mutations.len();
        if mutant.mutations.len() == 1 {
            unbatched_mutations_count += 1;
        }

        if verbosity >= 1 {
            print!("{}: ", mutant.id.index());
        }
        match mutant.mutations.len() {
            1 => println!("1 mutation"),
            _ => println!("{} mutations", mutant.mutations.len()),
        };

        // Mutations are printed in assigned ID order.
        let mut mutations_in_print_order = mutant.mutations.iter().collect::<Vec<_>>();
        mutations_in_print_order.sort_unstable_by_key(|mutation| mutation.id.index());

        for mutation in mutations_in_print_order {
            let mut unsafe_marker = "";
            match (mutation.is_unsafe(unsafe_targeting), mutation.target.unsafety) {
                (true, Unsafety::Tainted(_)) => {
                    unsafe_mutations_count += 1;
                    tainted_mutations_count += 1;
                    unsafe_marker = "(tainted) ";
                }
                (true, _) => {
                    unsafe_mutations_count += 1;
                    unsafe_marker = "(unsafe) ";
                }
                (false, _) => {}
            };

            print!("  - ");
            if verbosity >= 1 {
                print!("{}: ", mutation.id.index());
            }
            println!("{unsafe_marker}[{op_name}] {display_name} in {def_path} at {display_location}",
                op_name = mutation.op_name(),
                display_name = mutation.display_name(),
                def_path = tcx.def_path_str(mutation.target.def_id.to_def_id()),
                display_location = mutation.display_location(tcx.sess),
            );

            // Entry points are printed in order of distance first, within that by lexical order of their definition path.
            let mut entry_points_in_print_order = mutation.target.reachable_from.iter()
                .map(|(&test, entry_point)| (test.path_str(), test, entry_point))
                .collect::<Vec<_>>();
            entry_points_in_print_order.sort_unstable_by(|(test_a_path_str, _, entry_point_a), (test_b_path_str, _, entry_point_b)| {
                Ord::cmp(&entry_point_a.distance, &entry_point_b.distance).then(Ord::cmp(test_a_path_str, test_b_path_str))
            });

            for (test_path_str, test, entry_point) in entry_points_in_print_order {
                println!("    <-({distance})- {tainted_marker}{test}",
                    distance = entry_point.distance,
                    tainted_marker = match mutation.target.is_tainted(test, unsafe_targeting) {
                        true => "(tainted) ",
                        false => "",
                    },
                    test = test_path_str,
                );
            }
        }

        println!();
    }

    if verbosity >= 1 {
        #[derive(Clone, Copy, Default)]
        struct MutationOpStats {
            total_mutations_count: usize,
            unsafe_mutations_count: usize,
            tainted_mutations_count: usize,
            unbatched_mutations_count: usize,
        }

        let mut mutation_op_stats: FxHashMap<&str, MutationOpStats> = Default::default();
        for mutant in mutants {
            for mutation in &mutant.mutations {
                let op_stats = mutation_op_stats.entry(mutation.op_name()).or_default();
                op_stats.total_mutations_count += 1;
                if mutation.is_unsafe(unsafe_targeting) { op_stats.unsafe_mutations_count += 1; }
                if let Unsafety::Tainted(_) = mutation.target.unsafety { op_stats.tainted_mutations_count +=1; }
                if mutant.mutations.len() == 1 { op_stats.unbatched_mutations_count += 1; }
            }
        }

        let op_name_w = mutest_operators::ALL.iter().map(|s| s.len()).max().unwrap_or(0);
        let mutations_w = mutation_op_stats.values().map(|s| s.total_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let safe_w = mutation_op_stats.values().map(|s| (s.total_mutations_count - s.unsafe_mutations_count).checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let unsafe_w = mutation_op_stats.values().map(|s| s.unsafe_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let tainted_w = mutation_op_stats.values().map(|s| s.tainted_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let batched_w = mutation_op_stats.values().map(|s| (s.total_mutations_count - s.unbatched_mutations_count).checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);
        let unbatched_w = mutation_op_stats.values().map(|s| s.unbatched_mutations_count.checked_ilog10().unwrap_or(0) as usize + 1).max().unwrap_or(0);

        // TODO: Only list statistics for active mutation operators.
        for op_name in mutest_operators::ALL {
            let op_stats = mutation_op_stats.get(op_name).map(|s| *s).unwrap_or_default();

            println!("{op_name:>op_name_w$}: {mutations_pct:>6}. {mutations:>mutations_w$} mutations; {safe:>safe_w$} safe; {unsafe:>unsafe_w$} unsafe ({tainted:>tainted_w$} tainted); {batched:>batched_w$} batched; {unbatched:>unbatched_w$} unbatched",
                mutations_pct = format!("{:.2}%", op_stats.total_mutations_count as f64 / total_mutations_count as f64 * 100_f64),
                mutations = op_stats.total_mutations_count,
                safe = op_stats.total_mutations_count - op_stats.unsafe_mutations_count,
                r#unsafe = op_stats.unsafe_mutations_count,
                tainted = op_stats.tainted_mutations_count,
                batched = op_stats.total_mutations_count - op_stats.unbatched_mutations_count,
                unbatched = op_stats.unbatched_mutations_count,
            );
        }

        println!();
    }

    println!("{mutants} mutants; {mutations} mutations; {safe} safe; {unsafe} unsafe ({tainted} tainted); {batched} batched; {unbatched} unbatched",
        mutants = mutants.len(),
        mutations = total_mutations_count,
        safe = total_mutations_count - unsafe_mutations_count,
        r#unsafe = unsafe_mutations_count,
        tainted = tainted_mutations_count,
        batched = total_mutations_count - unbatched_mutations_count,
        unbatched = unbatched_mutations_count,
    );
}
