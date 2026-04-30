use std::borrow::Cow;
use std::collections::BTreeMap;
use std::cmp::Ordering;
use std::fmt::Write;
use std::iter;
use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Redirect, Response};
use serde::Deserialize;

use mutest_json::{DefId, Span};
use mutest_json::mutations::{MutationId, TargetId};

use crate::call_graph::{TraceSpec, entry_point_mono_call_traces, reduce_mono_call_traces};
use crate::config::Options;
use crate::ctxt::{TargetSpec, WorkspaceCtxt};
use crate::evaluation::{TestMutationResult, MutationDetection};
use crate::html::source_code_line_content;
use crate::html::base::{Tab, base_html, topbar_html};
use crate::html::mutations::{OverlappingGroupOfMutations, OverlappingGroupKind};
use crate::source_file::{LineNo, TreeEntry, file_tree_entries, line_region_byte_offsets_within_span};

pub(crate) struct ServerState {
    pub wcx: WorkspaceCtxt,
}

pub(crate) async fn run(opts: &Options, state: ServerState) {
    let state = Arc::new(state);

    let router = axum::Router::new()
        .route("/", axum::routing::get(handle_root_request))
        .nest("/static", memory_serve::load!().into_router())
        .nest("/{package}/{target}", axum::Router::new()
            .route("/", axum::routing::get(handle_target_root_request))
            .route("/source", axum::routing::get(handle_source_request))
            .route("/source/{*path}", axum::routing::get(handle_source_request))
            .route("/mutations", axum::routing::get(handle_mutations_request))
            .route("/mutations/{mutation_id}", axum::routing::get(handle_mutation_request))
            .route("/traces/{trace_spec}", axum::routing::get(handle_trace_request))
            .route("/tests", axum::routing::get(handle_tests_request))
            .route("/tests/{test_name}", axum::routing::get(handle_test_request))
        )
        .fallback(handle_unknown)
        .with_state(state);

    let listener = tokio::net::TcpListener::bind(("0.0.0.0", opts.port)).await.expect("cannot bind port");
    println!("listening on 0.0.0.0:{}", opts.port);
    println!("visit http://127.0.0.1:{}", opts.port);
    axum::serve(listener, router).await.unwrap();
}

async fn handle_unknown() -> impl IntoResponse {
    (StatusCode::NOT_FOUND, "404")
}

async fn handle_root_request(State(state): State<Arc<ServerState>>) -> Response {
    let wcx = &state.wcx;
    let [(package, target), ..] = wcx.targets() else {
        return (StatusCode::OK, "no targets loaded").into_response();
    };
    Redirect::to(&format!("/{}/{}", package, target.path_str())).into_response()
}

async fn handle_target_root_request(Path((package, target)): Path<(String, TargetSpec)>) -> Response {
    Redirect::to(&format!("/{}/{}/source", package, target.path_str())).into_response()
}

// NOTE: This struct is required because an optional trailing path segment cannot be declared in tuple form.
#[derive(Deserialize)]
struct SourceRequestPathParams {
    package: String,
    target: TargetSpec,
    path: Option<PathBuf>,
}

async fn handle_source_request(State(state): State<Arc<ServerState>>, Path(path_params): Path<SourceRequestPathParams>) -> (StatusCode, Html<String>) {
    let SourceRequestPathParams { package, target, path } = path_params;

    let wcx = &state.wcx;

    let target_path = format!("{}/{}", package, target.path_str());
    let Some(tcx) = wcx.target(&package, &target) else {
        return (StatusCode::NOT_FOUND, Html(format!("target `{}` not found", target_path)));
    };

    let evaluation_info = tcx.evaluation_info();

    let mut body = String::new();

    let title = match &path {
        Some(path) => &path.to_string_lossy(),
        None => "source",
    };
    base_html(&mut body, title).unwrap();
    write!(body, "<body>").unwrap();

    topbar_html(&mut body, wcx, &package, &target, tcx, Some(Tab::Sources)).unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    write!(body, "<nav class=\"sidebar\">").unwrap();
    write!(body, "<ol class=\"file-tree\">").unwrap();
    for tree_entry in file_tree_entries(tcx.local_source_file_paths()) {
        match tree_entry {
            TreeEntry::Dir(entry_path) => {
                let Some(dir_name) = entry_path.file_name() else {
                    return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid path amongst file tree entries: `{}`", entry_path.display())));
                };
                write!(body, "<li><a class=\"item\"><span class=\"name\">{}/</span></a><ol>", dir_name.display()).unwrap();
            }
            TreeEntry::File(entry_path) => {
                let Some(file_name) = entry_path.file_name() else {
                    return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid path amongst file tree entries: `{}`", entry_path.display())));
                };
                let file_mutations = tcx.file_mutations(entry_path);

                write!(body, "<li><a class=\"item").unwrap();
                if let Some(path) = &path && entry_path == path {
                    write!(body, " selected").unwrap();
                }
                write!(body, "\" href=\"/{}/source/{}\">", target_path, entry_path.display()).unwrap();
                write!(body, "<span class=\"name\">{}</span>", file_name.display()).unwrap();
                write!(body, "<div class=\"badge\">").unwrap();
                if let Some(evaluation_info) = evaluation_info {
                    let undetected_mutations_count = file_mutations.iter()
                        .filter(|&&mutation_id| matches!(evaluation_info.mutation_detections[mutation_id], MutationDetection::Undetected))
                        .count();
                    if undetected_mutations_count >= 1 {
                        write!(body, "<span class=\"undetected\">{}</span>", undetected_mutations_count).unwrap();
                    }
                }
                write!(body, "<span class=\"total\">{}</span>", file_mutations.len()).unwrap();
                write!(body, "</div>").unwrap();
                write!(body, "</a></li>").unwrap();
            }
            TreeEntry::EndDir => {
                write!(body, "</ol></li>").unwrap();
            }
        }
    }
    write!(body, "</ol>").unwrap();
    write!(body, "</nav>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

    let Some(path) = path else {
        // NOTE: Empty main content for root source page.
        write!(body, "</main").unwrap();
        write!(body, "</div>").unwrap();
        write!(body, "</body>").unwrap();
        write!(body, "</html>").unwrap();
        return (StatusCode::OK, Html(body));
    };

    let Some(source_file_html) = wcx.source_file_html(&path) else {
        return (StatusCode::NOT_FOUND, Html(format!("source file `{}` not found", path.display())));
    };

    let overlapping_groups_of_mutations_in_file = tcx.overlapping_groups_of_mutations_in_file(&path);

    let mut total_mutations_count = 0;
    let mut undetected_mutations_count = 0;
    let mut detected_mutations_count = 0;
    let mut timed_out_mutations_count = 0;
    let mut crashed_mutations_count = 0;
    for group in overlapping_groups_of_mutations_in_file {
        for &mutation_id in &group.mutations {
            match evaluation_info {
                Some(evaluation_info) => {
                    match evaluation_info.mutation_detections[mutation_id] {
                        MutationDetection::Undetected => undetected_mutations_count += 1,
                        MutationDetection::Detected => detected_mutations_count += 1,
                        MutationDetection::TimedOut => timed_out_mutations_count += 1,
                        MutationDetection::Crashed => crashed_mutations_count += 1,
                    }
                }
                None => {}
            }

            total_mutations_count += 1;
        }
    }

    write!(body, "<header>").unwrap();
    write!(body, "<h1>{}</h1>", path.display()).unwrap();
    match total_mutations_count {
        0 => {
            write!(body, "<div class=\"inline-stats none\">").unwrap();
            write!(body, "<span class=\"label\">no mutations</span>").unwrap();
            write!(body, "</div>").unwrap();
        }
        _ => {
            let coverage = (total_mutations_count - undetected_mutations_count) as f64 / total_mutations_count as f64;

            write!(body, "<div class=\"inline-stats\">").unwrap();
            write!(body, "<span class=\"value total\">{}</span><span class=\"label\">mutations</span>", total_mutations_count).unwrap();
            match evaluation_info {
                None => write!(body, "<span class=\"value coverage\">unknown</span><span class=\"label\">coverage</span>").unwrap(),
                Some(_) => write!(body, "<span class=\"value coverage\">{:.2}%</span><span class=\"label\">coverage</span>", coverage * 100_f64).unwrap(),
            }
            write!(body, "<span class=\"value undetected\">{}</span><span class=\"label\">undetected</span>", undetected_mutations_count).unwrap();
            write!(body, "<span class=\"value detected\">{}</span><span class=\"label\">detected</span>", detected_mutations_count).unwrap();
            write!(body, "<span class=\"value timed-out\">{}</span><span class=\"label\">timed out</span>", timed_out_mutations_count).unwrap();
            write!(body, "<span class=\"value crashed\">{}</span><span class=\"label\">crashed</span>", crashed_mutations_count).unwrap();
            write!(body, "</div>").unwrap();
        }
    }
    write!(body, "</header>").unwrap();

    write!(body, "<table class=\"source-code\">").unwrap();
    let mut highlighted_lines_html_iter = source_file_html.highlighted_lines_html.iter_enumerated();
    while let Some((line_no, highlighted_line_html)) = highlighted_lines_html_iter.next() {
        let mut groups_starting_on_current_line = overlapping_groups_of_mutations_in_file.iter().enumerate()
            .filter(|(_, g)| g.start_line == line_no)
            .map(|(idx, g)| (idx + 1, g))
            .peekable();

        if groups_starting_on_current_line.clone().filter(|(_, g)| g.kind == OverlappingGroupKind::InsertBefore).count() > 1 {
            return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid grouping of overlapping mutations for source file `{}`", path.display())));
        }
        if groups_starting_on_current_line.clone().filter(|(_, g)| g.kind == OverlappingGroupKind::Replace).count() > 1 {
            return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid grouping of overlapping mutations for source file `{}`", path.display())));
        }
        if groups_starting_on_current_line.clone().filter(|(_, g)| g.kind == OverlappingGroupKind::InsertAfter).count() > 1 {
            return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid grouping of overlapping mutations for source file `{}`", path.display())));
        }

        if let None = groups_starting_on_current_line.peek() {
            let line_content = source_code_line_content(highlighted_line_html.as_str());
            write!(body, "<tbody class=\"line\"><tr id=\"L{}\" class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr></tbody>", line_no.0, line_no.0, line_content).unwrap();
            continue;
        }

        let mut write_overlapping_mutation_group = |body: &mut String, local_group_id: usize, g: &OverlappingGroupOfMutations| {
            let group_heading = format!("{mutations} {action} {lines}",
                mutations = match g.mutations.len() {
                    1 => "1 mutation".to_owned(),
                    n => format!("{n} mutations"),
                },
                action = match g.kind {
                    OverlappingGroupKind::InsertBefore => "inserting before",
                    OverlappingGroupKind::Replace => "replacing",
                    OverlappingGroupKind::InsertAfter => "inserting after",
                },
                lines = match g.end_line.0 - g.start_line.0 + 1 {
                    1 => format!("line {}", g.start_line.0),
                    _ => format!("lines {} to {}", g.start_line.0, g.end_line.0),
                },
            );

            write!(body, "<tbody data-group-id=\"{local_group_id}\" class=\"group\"><tr class=\"heading\"><td colspan=\"2\">{}</td></tr>", group_heading).unwrap();
            // Write out original lines of code that this overlapping group of mutations replaces.
            if g.kind == OverlappingGroupKind::Replace {
                for (line_no, highlighted_line_html) in iter::zip(g.start_line.0..=g.end_line.0, &source_file_html.highlighted_lines_html[g.start_line..=g.end_line]) {
                    let line_content = match highlighted_line_html.as_str().is_empty() {
                        false => highlighted_line_html.as_str(),
                        true => "\n",
                    };
                    write!(body, "<tr id=\"L{}\" class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_no, line_content).unwrap();
                }

                // Skip remaining source lines covered by the group.
                // NOTE: The first source line has already been taken from the iterator,
                //       so we must only move the cursor for the remaining source lines.
                for _ in g.start_line.0..g.end_line.0 { let _ = highlighted_lines_html_iter.next(); }
            }

            write!(body, "</tbody>").unwrap();

            for &mutation_id in &g.mutations {
                let Some(mutation) = tcx.mutation(mutation_id) else { continue; };

                let mutation_detection_html = match evaluation_info {
                    Some(evaluation_info) => evaluation_info.mutation_detections[mutation_id].badge_html(),
                    None => "",
                };

                // Write out mutation heading.
                write!(body, "<tbody data-group-id=\"{local_group_id}\" id=\"M{}\" class=\"mutation\"><tr class=\"heading\"><td colspan=\"2\">{} <a href=\"/{}/mutations/{}\">mutation {}</a>: {}</td></tr>", mutation_id.0, mutation_detection_html, target_path, mutation_id.0, mutation_id.0, mutation.display_name_html).unwrap();

                // Filter to relevant substitutions that fall within this overlap group.
                let substs_within_group = mutation.subst_htmls.iter().filter(|subst_html| {
                    g.overlaps(subst_html.start_line, subst_html.end_line())
                });

                for subst_html in substs_within_group {
                    let subst_start_line = subst_html.start_line;
                    let subst_end_line = subst_html.end_line();

                    // Write out prefix lines that are part of the group but are not modified by this particular mutation substitution.
                    if g.kind == OverlappingGroupKind::Replace {
                        let before_subst_start_line = LineNo(subst_start_line.0 - 1);
                        for (line_no, highlighted_line_html) in iter::zip(g.start_line.0.., &source_file_html.highlighted_lines_html[g.start_line..=before_subst_start_line]) {
                            let line_content = source_code_line_content(highlighted_line_html.as_str());
                            write!(body, "<tr class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                        }
                    }
                    // Write out the original lines that this mutation subsitution modifies.
                    for (line_no, highlighted_line_html) in iter::zip(subst_start_line.0.., &subst_html.original_lines_html) {
                        let line_content = source_code_line_content(highlighted_line_html.as_str());
                        write!(body, "<tr class=\"line original\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                    }
                    // Write out replacement lines.
                    for highlighted_line_html in &subst_html.replacement_lines_html {
                        let line_content = source_code_line_content(highlighted_line_html.as_str());
                        write!(body, "<tr class=\"line mutated\"><td></td><td class=\"line-content\">{}</td></tr>", line_content).unwrap();
                    }
                    // Write out suffix lines that are part of the group but are not modified by this particular mutation substitution.
                    if g.kind == OverlappingGroupKind::Replace {
                        let after_subst_start_line = LineNo(subst_end_line.0 + 1);
                        for (line_no, highlighted_line_html) in iter::zip(after_subst_start_line.0.., &source_file_html.highlighted_lines_html[after_subst_start_line..=g.end_line]) {
                            let line_content = source_code_line_content(highlighted_line_html.as_str());
                            write!(body, "<tr class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                        }
                    }
                }
            }
            write!(body, "</tbody>").unwrap();
        };

        let mut insert_before = groups_starting_on_current_line.clone().filter(|(_, g)| g.kind == OverlappingGroupKind::InsertBefore);
        if let Some((local_group_id, group)) = insert_before.next() {
            if let Some(_) = insert_before.next() {
                return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid grouping of overlapping mutations for source file `{}`", path.display())));
            }
            write_overlapping_mutation_group(&mut body, local_group_id, group);
        }

        let mut replace = groups_starting_on_current_line.clone().filter(|(_, g)| g.kind == OverlappingGroupKind::Replace);
        if let Some((local_group_id, group)) = replace.next() {
            if let Some(_) = replace.next() {
                return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid grouping of overlapping mutations for source file `{}`", path.display())));
            }
            write_overlapping_mutation_group(&mut body, local_group_id, group);
        } else {
            let line_content = match highlighted_line_html.as_str().is_empty() {
                false => highlighted_line_html.as_str(),
                true => "\n",
            };
            write!(body, "<tbody class=\"line\"><tr id=\"L{}\" class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr></tbody>", line_no.0, line_no.0, line_content).unwrap();
        }

        let mut insert_after = groups_starting_on_current_line.clone().filter(|(_, g)| g.kind == OverlappingGroupKind::InsertAfter);
        if let Some((local_group_id, group)) = insert_after.next() {
            if let Some(_) = insert_after.next() {
                return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid grouping of overlapping mutations for source file `{}`", path.display())));
            }
            write_overlapping_mutation_group(&mut body, local_group_id, group);
        }
    }
    write!(body, "</table>").unwrap();

    write!(body, "</main").unwrap();

    write!(body, "</div>").unwrap();

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}

async fn handle_mutations_request(State(state): State<Arc<ServerState>>, Path((package, target)): Path<(String, TargetSpec)>) -> (StatusCode, Html<String>) {
    let wcx = &state.wcx;

    let target_path = format!("{}/{}", package, target.path_str());
    let Some(tcx) = wcx.target(&package, &target) else {
        return (StatusCode::NOT_FOUND, Html(format!("target `{}` not found", target_path)));
    };

    let evaluation_info = tcx.evaluation_info();

    let mut body = String::new();

    base_html(&mut body, "mutations").unwrap();
    write!(body, "<body>").unwrap();

    topbar_html(&mut body, wcx, &package, &target, tcx, Some(Tab::Mutations)).unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    // NOTE: No sidebar, emit placeholder element.
    write!(body, "<div></div>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

    let total_mutations_count = tcx.mutations_count();
    let mut undetected_mutations_count = 0;
    let mut detected_mutations_count = 0;
    let mut timed_out_mutations_count = 0;
    let mut crashed_mutations_count = 0;
    match evaluation_info {
        Some(evaluation_info) => {
            for mutation_detection in &evaluation_info.mutation_detections {
                match mutation_detection {
                    MutationDetection::Undetected => undetected_mutations_count += 1,
                    MutationDetection::Detected => detected_mutations_count += 1,
                    MutationDetection::TimedOut => timed_out_mutations_count += 1,
                    MutationDetection::Crashed => crashed_mutations_count += 1,
                }
            }
        }
        None => {}
    }

    write!(body, "<header>").unwrap();
    match total_mutations_count {
        0 => {
            write!(body, "<div class=\"inline-stats none\">").unwrap();
            write!(body, "<span class=\"label\">no mutations</span>").unwrap();
            write!(body, "</div>").unwrap();
        }
        _ => {
            let coverage = (total_mutations_count - undetected_mutations_count) as f64 / total_mutations_count as f64;

            write!(body, "<div class=\"inline-stats\">").unwrap();
            write!(body, "<span class=\"value total\">{}</span><span class=\"label\">mutations</span>", total_mutations_count).unwrap();
            match evaluation_info {
                None => write!(body, "<span class=\"value coverage\">unknown</span><span class=\"label\">coverage</span>").unwrap(),
                Some(_) => write!(body, "<span class=\"value coverage\">{:.2}%</span><span class=\"label\">coverage</span>", coverage * 100_f64).unwrap(),
            }
            write!(body, "<span class=\"value undetected\">{}</span><span class=\"label\">undetected</span>", undetected_mutations_count).unwrap();
            write!(body, "<span class=\"value detected\">{}</span><span class=\"label\">detected</span>", detected_mutations_count).unwrap();
            write!(body, "<span class=\"value timed-out\">{}</span><span class=\"label\">timed out</span>", timed_out_mutations_count).unwrap();
            write!(body, "<span class=\"value crashed\">{}</span><span class=\"label\">crashed</span>", crashed_mutations_count).unwrap();
            write!(body, "</div>").unwrap();
        }
    }
    write!(body, "</header>").unwrap();

    write!(body, "<table class=\"page-table\">").unwrap();
    write!(body, "<tbody>").unwrap();
    for file_path in tcx.local_source_file_paths() {
        write!(body, "<tr class=\"heading\"><th colspan=\"3\">{}</th></tr>", file_path.display()).unwrap();

        for &mutation_id in tcx.file_mutations(file_path) {
            let Some(mutation) = tcx.mutation(mutation_id) else { continue; };

            let mutation_detection_html = match evaluation_info {
                Some(evaluation_info) => evaluation_info.mutation_detections[mutation_id].badge_html(),
                None => "",
            };

            let source_path_str = mutation.origin_span.path.to_string_lossy();
            let (lo_line, lo_col) = mutation.origin_span.begin;
            let (hi_line, hi_col) = mutation.origin_span.end;

            write!(body, "<tr id=\"M{}\" class=\"entry\">", mutation_id.0).unwrap();
            write!(body, "<td class=\"right loc\"><a href=\"/{}/source/{}#M{}\">{}:{} {}:{}</a></td>", target_path, source_path_str, mutation_id.0, lo_line, lo_col, hi_line, hi_col).unwrap();
            write!(body, "<td class=\"right right-tight\">{}</td>", mutation_detection_html).unwrap();
            write!(body, "<td class=\"expand desc\"><span><a href=\"/{}/mutations/{}\">mutation {}</a>: {}</span></td>", target_path, mutation_id.0, mutation_id.0, mutation.display_name_html).unwrap();
            write!(body, "</tr>").unwrap();
        }
    }
    write!(body, "</tbody></table>").unwrap();

    write!(body, "</main>").unwrap();

    write!(body, "</div>").unwrap();

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}

async fn handle_mutation_request(State(state): State<Arc<ServerState>>, Path((package, target, mutation_id)): Path<(String, TargetSpec, MutationId)>) -> (StatusCode, Html<String>) {
    let wcx = &state.wcx;

    let target_path = format!("{}/{}", package, target.path_str());
    let Some(tcx) = wcx.target(&package, &target) else {
        return (StatusCode::NOT_FOUND, Html(format!("target `{}` not found", target_path)));
    };

    let Some(mutation) = tcx.mutation(mutation_id) else {
        return (StatusCode::NOT_FOUND, Html(format!("mutation `{}` not found", mutation_id.0)));
    };
    let Some(mutation_target) = tcx.target(mutation.target_id) else {
        return (StatusCode::NOT_FOUND, Html(format!("target of mutation `{}` not found", mutation_id.0)));
    };
    let Some(def) = tcx.definition(mutation_target.def_id) else {
        return (StatusCode::NOT_FOUND, Html(format!("target definition of mutation `{}` not found", mutation_id.0)));
    };

    let Some(source_file_html) = wcx.source_file_html(&mutation.origin_span.path) else {
        return (StatusCode::NOT_FOUND, Html(format!("source file `{}` not found", mutation.origin_span.path.display())));
    };

    let call_graph = tcx.call_graph();
    let evaluation_info = tcx.evaluation_info();

    let mut body = String::new();

    base_html(&mut body, &format!("mutation {}: {}", mutation_id.0, mutation.display_name)).unwrap();
    write!(body, "<body>").unwrap();

    topbar_html(&mut body, wcx, &package, &target, tcx, None).unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    // NOTE: No sidebar, emit placeholder element.
    write!(body, "<div></div>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

    let mutation_detection_html = match evaluation_info {
        Some(evaluation_info) => evaluation_info.mutation_detections[mutation_id].badge_html(),
        None => "",
    };

    write!(body, "<header>").unwrap();
    write!(body, "<h1>{} mutation {}: {}</h1>", mutation_detection_html, mutation_id.0, mutation.display_name_html).unwrap();
    if let Some(def_span) = &def.span {
        write!(body, " <span>in <a href=\"/{}/source/{}#M{}\">{}</a></span>", target_path, def_span.path.display(), mutation_id.0, def_span.path.display()).unwrap();
    }
    write!(body, "</header>").unwrap();

    write!(body, "<section>").unwrap();
    write!(body, "<table class=\"source-code\">").unwrap();

    // Set start line for source snippet.
    let mut source_start_line = match def.display_start_line() {
        // Use target display start line, if available.
        Some(def_display_start_line) => def_display_start_line,
        // Fall back to the start line of the first substitution.
        None if let [first_subst_html, ..] = &mutation.subst_htmls[..] => first_subst_html.start_line,
        // Mutations without substitutions are not valid.
        _ => { return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("mutation `{}` is invalid: no substitutions", mutation_id.0))); }
    };

    for subst_html in &mutation.subst_htmls {
        let subst_start_line = subst_html.start_line;

        // Write out prefix source lines from end of previous segment (or start of target def).
        for (line_no, highlighted_line_html) in iter::zip(source_start_line.0.., &source_file_html.highlighted_lines_html[source_start_line..subst_start_line]) {
            let line_content = source_code_line_content(highlighted_line_html.as_str());
            write!(body, "<tr class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
        }
        // Write out the original lines that this mutation subsitution modifies.
        for (line_no, highlighted_line_html) in iter::zip(subst_start_line.0.., &subst_html.original_lines_html) {
            let line_content = source_code_line_content(highlighted_line_html.as_str());
            write!(body, "<tr class=\"line original\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
        }
        // Write out replacement lines.
        for highlighted_line_html in &subst_html.replacement_lines_html {
            let line_content = source_code_line_content(highlighted_line_html.as_str());
            write!(body, "<tr class=\"line mutated\"><td></td><td class=\"line-content\">{}</td></tr>", line_content).unwrap();
        }

        source_start_line = subst_html.after_source_line_no();
    }

    write!(body, "</table>").unwrap();
    write!(body, "</section>").unwrap();

    let mut total_tests_count = 0;
    let mut not_ran_tests_count = 0;
    let mut undetected_tests_count = 0;
    let mut detected_tests_count = 0;
    let mut timed_out_tests_count = 0;
    let mut crashed_tests_count = 0;
    for (def_path, _) in &mutation_target.reachable_from {
        match evaluation_info {
            Some(evaluation_info) => {
                let Some(test_runs) = evaluation_info.test_runs.get(def_path) else { continue; };

                match test_runs.mutation_detections[mutation_id] {
                    None => continue,
                    Some(TestMutationResult::NotRan) => not_ran_tests_count += 1,
                    Some(TestMutationResult::Ran(MutationDetection::Undetected)) => undetected_tests_count += 1,
                    Some(TestMutationResult::Ran(MutationDetection::Detected)) => detected_tests_count += 1,
                    Some(TestMutationResult::Ran(MutationDetection::TimedOut)) => timed_out_tests_count += 1,
                    Some(TestMutationResult::Ran(MutationDetection::Crashed)) => crashed_tests_count += 1,
                }
            }
            None => not_ran_tests_count += 1,
        }

        total_tests_count += 1;
    }

    write!(body, "<section class=\"text\">").unwrap();
    write!(body, "<h2>tests</h2>").unwrap();
    match total_tests_count {
        0 => {
            write!(body, "<p>mutation not reached by any test</p>").unwrap();
        }
        _ => {
            let ran_tests_count = total_tests_count - not_ran_tests_count;
            let coverage = (ran_tests_count - undetected_tests_count) as f64 / ran_tests_count as f64;

            write!(body, "<div class=\"labeled-stats\">").unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">reached by</div><div class=\"value\">{}</div></div>", total_tests_count).unwrap();
            write!(body, "<div class=\"divider\"></div>").unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">ran</div><div class=\"value\">{}</div></div>", ran_tests_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">skipped</div><div class=\"value\">{}</div></div>", not_ran_tests_count).unwrap();
            write!(body, "<div class=\"divider\"></div>").unwrap();
            match ran_tests_count {
                0 => write!(body, "<div class=\"stat\"><div class=\"label\">coverage</div><div class=\"value\">unknown</div></div>").unwrap(),
                _ => write!(body, "<div class=\"stat\"><div class=\"label\">coverage</div><div class=\"value\">{:.2}%</div></div>", coverage * 100_f64).unwrap(),
            }
            write!(body, "<div class=\"stat\"><div class=\"label\">undetected</div><div class=\"value\">{}</div></div>", undetected_tests_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">detected</div><div class=\"value\">{}</div></div>", detected_tests_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">timed out</div><div class=\"value\">{}</div></div>", timed_out_tests_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">crashed</div><div class=\"value\">{}</div></div>", crashed_tests_count).unwrap();
            write!(body, "</div>").unwrap();
        }
    }
    write!(body, "</section>").unwrap();

    write!(body, "<section>").unwrap();
    write!(body, "<table class=\"page-table\">").unwrap();
    let mut reaching_def_paths = mutation_target.reachable_from.keys().collect::<Vec<_>>();
    reaching_def_paths.sort_unstable();
    for def_path in &reaching_def_paths {
        let Some(test) = tcx.test(def_path) else { continue; };
        let Some(test_def) = tcx.definition(test.def_id) else { continue; };

        let test_mutation_result = match evaluation_info {
            Some(evaluation_info) => evaluation_info.test_mutation_result(mutation_id, def_path).unwrap_or(TestMutationResult::NotRan),
            None => TestMutationResult::NotRan,
        };

        write!(body, "<tr class=\"heading\"><th class=\"right right-tight\">{}</th><th class=\"expand\"><a href=\"/{}/tests/{}\">{}</a></th></tr>", test_mutation_result.badge_html(), target_path, def_path, test_def.def_path_html).unwrap();

        let Some(entry_point) = call_graph.entry_points.iter().find(|entry_point| entry_point.def_id == test.def_id) else { continue; };

        let mono_call_traces = entry_point_mono_call_traces(&call_graph, entry_point, mutation_target.def_id);
        let mut def_call_traces = reduce_mono_call_traces(&call_graph, &mono_call_traces);

        def_call_traces.sort_unstable_by_key(|call_trace| call_trace.nested_calls.len());
        def_call_traces.sort_by(|call_trace_a, call_trace_b| {
            for (def_id_a, def_id_b) in iter::zip(call_trace_a.nested_calls.iter().rev(), call_trace_b.nested_calls.iter().rev()) {
                match Ord::cmp(&def_id_a.0, &def_id_b.0) {
                    Ordering::Equal => {}
                    ord => return ord,
                }
            }

            Ordering::Equal
        });

        for call_trace in &def_call_traces {
            let trace_spec = TraceSpec {
                entry_point_def_path: def_path,
                callee_def_ids: call_trace.nested_calls.clone(),
                mutation_id,
            };

            write!(body, "<tr><td class=\"right right-tight\"><a href=\"/{}/traces/", target_path).unwrap();
            trace_spec.write(&mut body);
            write!(body, "\">trace ({})</a>", call_trace.nested_calls.len()).unwrap();
            write!(body, "</td><td class=\"expand\"><span class=\"inline-trace\">").unwrap();

            let [in_between_callees @ .., _] = &call_trace.nested_calls[..] else {
                // NOTE: Not a valid case: there should always be at least one callee, the target, and we never mutate entry points.
                return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("invalid call trace `{}`: no calllees", trace_spec.to_string())));
            };

            for &def_id in in_between_callees {
                match tcx.definition(def_id) {
                    Some(def) => write!(body, "<span class=\"sep\">→</span> {}", def.def_path_html).unwrap(),
                    None => write!(body, "<span class=\"sep\">→</span> <i>unknown</i>").unwrap(),
                }
            }

            // NOTE: Since the last def is the same mutation target in all cases, we simplify how it is shown.
            write!(body, "<span class=\"sep\">→</span> <i>mutation</i>").unwrap();

            write!(body, "</span></td></tr>").unwrap();
        }
    }
    write!(body, "</table>").unwrap();
    write!(body, "</section>").unwrap();

    write!(body, "</main>").unwrap();

    write!(body, "</div>").unwrap();

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}

async fn handle_trace_request(State(state): State<Arc<ServerState>>, Path((package, target, trace_spec)): Path<(String, TargetSpec, String)>) -> (StatusCode, Html<String>) {
    let wcx = &state.wcx;

    let target_path = format!("{}/{}", package, target.path_str());
    let Some(tcx) = wcx.target(&package, &target) else {
        return (StatusCode::NOT_FOUND, Html(format!("target `{}` not found", target_path)));
    };

    let Some(trace_spec) = TraceSpec::parse(&trace_spec) else {
        return (StatusCode::BAD_REQUEST, Html(format!("invalid trace spec: `{}`", trace_spec)));
    };
    let Some(entry_point_def) = tcx.lookup_definition_by_path(trace_spec.entry_point_def_path) else {
        return (StatusCode::NOT_FOUND, Html(format!("definition `{}` not found", trace_spec.entry_point_def_path)));
    };
    let Some(target_def) = tcx.definition(trace_spec.target()) else {
        return (StatusCode::NOT_FOUND, Html(format!("definition `{:?}` not found", trace_spec.target())));
    };

    let call_graph = tcx.call_graph();
    let evaluation_info = tcx.evaluation_info();

    let mut body = String::new();

    let mut title = String::new();
    write!(title, "trace ").unwrap();
    match trace_spec.callee_def_ids.len() {
        1 => write!(title, "1 call").unwrap(),
        count => write!(title, "{} calls", count).unwrap(),
    }
    write!(title, " deep from `{}` to `{}` with mutation {}", entry_point_def.def_path, target_def.def_path, trace_spec.mutation_id.0).unwrap();
    base_html(&mut body, &title).unwrap();
    write!(body, "<body>").unwrap();

    topbar_html(&mut body, wcx, &package, &target, tcx, None).unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    // NOTE: No sidebar, emit placeholder element.
    write!(body, "<div></div>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

    write!(body, "<header>").unwrap();
    write!(body, "<h1>").unwrap();
    // NOTE: This is technically the detection status of the mutation for the test along any call trace, not just this one.
    if let Some(evaluation_info) = evaluation_info && let Some(test_mutation_result) = evaluation_info.test_mutation_result(trace_spec.mutation_id, &entry_point_def.def_path) {
        write!(body, "{} ", test_mutation_result.badge_html()).unwrap();
    }
    write!(body, "trace ").unwrap();
    match trace_spec.callee_def_ids.len() {
        1 => write!(body, "1 call").unwrap(),
        count => write!(body, "{} calls", count).unwrap(),
    }
    write!(body, " deep from <span class=\"inline-code\"><a href=\"/{}/tests/{}\">{}</a></span>", target_path, entry_point_def.def_path, entry_point_def.def_path_html).unwrap();
    write!(body, " to <span class=\"inline-code\">{}</span>", target_def.def_path_html).unwrap();
    write!(body, " with <a href=\"/{}/mutations/{}\">mutation {}</a></h1>", target_path, trace_spec.mutation_id.0, trace_spec.mutation_id.0).unwrap();
    write!(body, "</header>").unwrap();

    enum TraceSnippetKind<'a> {
        Calls {
            callee_def_id: DefId,
            call_spans: Vec<&'a Span>,
        },
        Mutation {
            mutation_id: MutationId,
        },
    }

    struct TraceSnippet<'a> {
        def_id: DefId,
        kind: TraceSnippetKind<'a>,
    }

    let mut trace_snippets = Vec::with_capacity(trace_spec.callee_def_ids.len() + 1);

    let Some(entry_point) = call_graph.entry_points.iter().find(|entry_point| entry_point.def_id == entry_point_def.def_id) else {
        return (StatusCode::BAD_REQUEST, Html(format!("definition `{}` not an entry point", entry_point_def.def_path)));
    };

    let mut call_spans = entry_point.calls.iter()
        .filter(|&(&callee_id, _)| call_graph.callees[callee_id].def_id == trace_spec.root_callee())
        .flat_map(|(_, calls)| calls.iter().flat_map(|call| &call.span))
        .collect::<Vec<_>>();
    call_spans.sort_by(|a, b| Ord::cmp(&a.end.0, &b.end.0));
    call_spans.dedup();

    let Some(root_callee_def) = tcx.definition(trace_spec.root_callee()) else { unreachable!(); };

    trace_snippets.push(TraceSnippet {
        def_id: entry_point_def.def_id,
        kind: TraceSnippetKind::Calls {
            callee_def_id: root_callee_def.def_id,
            call_spans,
        },
    });

    for &[def_id, next_def_id] in trace_spec.callee_def_ids.array_windows::<2>() {
        let mut call_spans = call_graph.callees.iter()
            .filter(|caller| caller.def_id == def_id)
            .flat_map(|caller| caller.calls.iter().filter(|&(&callee_id, _)| call_graph.callees[callee_id].def_id == next_def_id))
            .flat_map(|(_, calls)| calls.iter().flat_map(|call| &call.span))
            .collect::<Vec<_>>();
        call_spans.sort_by(|a, b| Ord::cmp(&a.end.0, &b.end.0));
        call_spans.dedup();

        trace_snippets.push(TraceSnippet {
            def_id,
            kind: TraceSnippetKind::Calls {
                callee_def_id: next_def_id,
                call_spans,
            },
        });
    }

    trace_snippets.push(TraceSnippet {
        def_id: trace_spec.target(),
        kind: TraceSnippetKind::Mutation { mutation_id: trace_spec.mutation_id },
    });

    for trace_snippet in trace_snippets {
        let Some(def) = tcx.definition(trace_snippet.def_id) else { continue; };

        write!(body, "<section class=\"trace-snippet\">").unwrap();

        write!(body, "<header>").unwrap();
        write!(body, "<span>").unwrap();
        if let Some(def_span) = &def.span {
            let Some(def_display_start_line) = def.display_start_line() else { unreachable!("definition with span has no display start line") };
            write!(body, "<a href=\"/{}/source/{}#L{}\">{}</a>: ", target_path, def_span.path.display(), def_display_start_line.0, def_span.path.display()).unwrap();
        }
        write!(body, "<span class=\"inline-code\">{}</span>", def.def_path_html).unwrap();
        match &trace_snippet.kind {
            &TraceSnippetKind::Calls { callee_def_id, ref call_spans } => {
                let Some(callee_def) = tcx.definition(callee_def_id) else { continue };

                write!(body, " calls ").unwrap();
                write!(body, "<span class=\"inline-code\">{}</span>", callee_def.def_path_html).unwrap();
                if let Some(_) = &def.span {
                    match call_spans.len() {
                        1 => write!(body, " in 1 place").unwrap(),
                        count => write!(body, " in {} places", count).unwrap(),
                    }
                }
            }
            &TraceSnippetKind::Mutation { mutation_id } => {
                let Some(mutation) = tcx.mutation(mutation_id) else {
                    return (StatusCode::NOT_FOUND, Html(format!("mutation `{}` not found", mutation_id.0)));
                };

                write!(body, " with <a href=\"/{}/mutations/{}\">mutation {}</a>: {}", target_path, mutation_id.0, mutation_id.0, mutation.display_name_html).unwrap();
            }
        }
        write!(body, "</span>").unwrap();
        write!(body, "</header>").unwrap();

        let Some(def_span) = &def.span else {
            write!(body, "<div class=\"notice\">source location not available</div>").unwrap();
            write!(body, "</section>").unwrap();
            continue;
        };

        let Some(source_file) = wcx.loaded_source_file(&def_span.path) else {
            write!(body, "<div class=\"notice\">source file `{}` not found</div>", def_span.path.display()).unwrap();
            write!(body, "</section>").unwrap();
            continue;
        };
        let Some(source_file_html) = wcx.source_file_html(&def_span.path) else {
            return (StatusCode::NOT_FOUND, Html(format!("source file `{}` not rendered", def_span.path.display())));
        };

        write!(body, "<table class=\"source-code\">").unwrap();
        let Some(start_line) = def.display_start_line() else { unreachable!("definition with span has no display start line") };
        match &trace_snippet.kind {
            TraceSnippetKind::Calls { callee_def_id: _, call_spans } => {
                let end_line = match call_spans.last() {
                    Some(last_call_span) => LineNo(last_call_span.end.0 as u32),
                    None => match &def.span {
                        Some(def_span) => LineNo(def_span.end.0 as u32),
                        None => start_line,
                    },
                };

                for (line_no, highlighted_line_html) in iter::zip(start_line.0.., &source_file_html.highlighted_lines_html[start_line..=end_line]) {
                    let def_line_region = line_region_byte_offsets_within_span(LineNo(line_no), &source_file.lines[LineNo(line_no)], def_span);
                    let call_line_regions = call_spans.iter().flat_map(|call_span| line_region_byte_offsets_within_span(LineNo(line_no), &source_file.lines[LineNo(line_no)], call_span));

                    let mut highlighted_line_html = Cow::Borrowed(highlighted_line_html);
                    if let Some((start_offset, end_offset)) = def_line_region {
                        highlighted_line_html.to_mut().insert_unbreakable_segment(start_offset, end_offset, "<span class=\"hi-def\">", "</span>");
                    }
                    for (start_offset, end_offset) in call_line_regions {
                        highlighted_line_html.to_mut().insert_unbreakable_segment(start_offset, end_offset, "<span class=\"hi-call\">", "</span>");
                    }

                    let line_content = source_code_line_content(highlighted_line_html.as_str());
                    write!(body, "<tr class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                }
            }
            &TraceSnippetKind::Mutation { mutation_id } => {
                let Some(mutation) = tcx.mutation(mutation_id) else {
                    return (StatusCode::NOT_FOUND, Html(format!("mutation `{}` not found", mutation_id.0)));
                };

                for subst_html in &mutation.subst_htmls {
                    let subst_start_line = subst_html.start_line;

                    let before_subst_start_line = LineNo(subst_start_line.0 - 1);

                    // Write out prefix source lines from start of target def.
                    for (line_no, highlighted_line_html) in iter::zip(start_line.0.., &source_file_html.highlighted_lines_html[start_line..=before_subst_start_line]) {
                        let def_line_region = line_region_byte_offsets_within_span(LineNo(line_no), &source_file.lines[LineNo(line_no)], def_span);
                        let mut highlighted_line_html = Cow::Borrowed(highlighted_line_html);
                        if let Some((start_offset, end_offset)) = def_line_region {
                            highlighted_line_html.to_mut().insert_unbreakable_segment(start_offset, end_offset, "<span class=\"hi-def\">", "</span>");
                        }
                        let line_content = source_code_line_content(highlighted_line_html.as_str());
                        write!(body, "<tr class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                    }
                    // Write out the original lines that this mutation subsitution modifies.
                    for (line_no, highlighted_line_html) in iter::zip(subst_start_line.0.., &subst_html.original_lines_html) {
                        let def_line_region = line_region_byte_offsets_within_span(LineNo(line_no), &source_file.lines[LineNo(line_no)], def_span);
                        let mut highlighted_line_html = Cow::Borrowed(highlighted_line_html);
                        if let Some((start_offset, end_offset)) = def_line_region {
                            highlighted_line_html.to_mut().insert_unbreakable_segment(start_offset, end_offset, "<span class=\"hi-def\">", "</span>");
                        }
                        let line_content = source_code_line_content(highlighted_line_html.as_str());
                        write!(body, "<tr class=\"line original\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                    }
                    // Write out replacement lines.
                    for highlighted_line_html in &subst_html.replacement_lines_html {
                        let line_content = source_code_line_content(highlighted_line_html.as_str());
                        write!(body, "<tr class=\"line mutated\"><td></td><td class=\"line-content\">{}</td></tr>", line_content).unwrap();
                    }
                }
            }
        }
        write!(body, "</table>").unwrap();

        write!(body, "</section>").unwrap();
    }

    write!(body, "</main>").unwrap();

    write!(body, "</div>").unwrap();

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}

async fn handle_tests_request(State(state): State<Arc<ServerState>>, Path((package, target)): Path<(String, TargetSpec)>) -> (StatusCode, Html<String>) {
    let wcx = &state.wcx;

    let target_path = format!("{}/{}", package, target.path_str());
    let Some(tcx) = wcx.target(&package, &target) else {
        return (StatusCode::NOT_FOUND, Html(format!("target `{}` not found", target_path)));
    };

    let tests = tcx.tests();

    let evaluation_info = tcx.evaluation_info();

    let mut body = String::new();

    base_html(&mut body, "tests").unwrap();
    write!(body, "<body>").unwrap();

    topbar_html(&mut body, wcx, &package, &target, tcx, Some(Tab::Tests)).unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    // NOTE: No sidebar, emit placeholder element.
    write!(body, "<div></div>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

    write!(body, "<table class=\"page-table\">").unwrap();
    write!(body, "<thead><tr><th>test</th><th class=\"right\">reachable mutations</th><th class=\"right\">ran</th><th class=\"right\">coverage</th><th class=\"right\">undetected</th><th class=\"right\">detected</th><th class=\"right\">timed out</th><th class=\"right\">crashed</th></tr></thead>").unwrap();
    write!(body, "<tbody>").unwrap();
    for test in tests {
        let Some(def) = tcx.definition(test.def_id) else {
            return (StatusCode::NOT_FOUND, Html(format!("definition `{:?}` not found", test.def_id)));
        };

        let mut total_mutations_count = 0;
        let mut not_ran_mutations_count = 0;
        let mut undetected_mutations_count = 0;
        let mut detected_mutations_count = 0;
        let mut timed_out_mutations_count = 0;
        let mut crashed_mutations_count = 0;
        let test_runs = evaluation_info.and_then(|evaluation_info| evaluation_info.test_runs.get(&def.def_path));
        for &target_id in tcx.targets_reached_by_test(&def.def_path) {
            for &mutation_id in tcx.target_mutations(target_id) {
                match test_runs {
                    Some(test_runs) => {
                        match test_runs.mutation_detections[mutation_id] {
                            None => continue,
                            Some(TestMutationResult::NotRan) => not_ran_mutations_count += 1,
                            Some(TestMutationResult::Ran(MutationDetection::Undetected)) => undetected_mutations_count += 1,
                            Some(TestMutationResult::Ran(MutationDetection::Detected)) => detected_mutations_count += 1,
                            Some(TestMutationResult::Ran(MutationDetection::TimedOut)) => timed_out_mutations_count += 1,
                            Some(TestMutationResult::Ran(MutationDetection::Crashed)) => crashed_mutations_count += 1,
                        }
                    }
                    None => not_ran_mutations_count += 1,
                }

                total_mutations_count += 1;
            }
        }

        write!(body, "<tr>").unwrap();
        write!(body, "<td class=\"expand sticky-content\"><a href=\"/{}/tests/{}\">{}</a></td>", target_path, def.def_path, def.def_path_html).unwrap();
        let ran_mutations_count = total_mutations_count - not_ran_mutations_count;
        match (total_mutations_count, ran_mutations_count) {
            (0, _) => {
                write!(body, "<td class=\"right\"><span class=\"value\">0</span></td>").unwrap();
                write!(body, "<td colspan=\"6\"></td>").unwrap();
            }
            (_, 0) => {
                write!(body, "<td class=\"right\"><span class=\"value\">{}</span></td>", total_mutations_count).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value\">0</span></td>").unwrap();
                write!(body, "<td colspan=\"5\"></td>").unwrap();
            }
            _ => {
                let coverage = (ran_mutations_count - undetected_mutations_count) as f64 / ran_mutations_count as f64;

                write!(body, "<td class=\"right\"><span class=\"value\">{}</span></td>", total_mutations_count).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value\">{}</span></td>", ran_mutations_count).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value\">{:.2}%</span></td>", coverage * 100_f64).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value undetected\">{}</span></td>", undetected_mutations_count).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value detected\">{}</span></td>", detected_mutations_count).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value timed-out\">{}</span></td>", timed_out_mutations_count).unwrap();
                write!(body, "<td class=\"right\"><span class=\"value crashed\">{}</span></td>", crashed_mutations_count).unwrap();
            }
        }
        write!(body, "</tr>").unwrap();
    }
    write!(body, "</tbody></table>").unwrap();

    write!(body, "</main>").unwrap();

    write!(body, "</div>").unwrap();

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}

async fn handle_test_request(State(state): State<Arc<ServerState>>, Path((package, target, test_name)): Path<(String, TargetSpec, String)>) -> (StatusCode, Html<String>) {
    let wcx = &state.wcx;

    let target_path = format!("{}/{}", package, target.path_str());
    let Some(tcx) = wcx.target(&package, &target) else {
        return (StatusCode::NOT_FOUND, Html(format!("target `{}` not found", target_path)));
    };

    let Some(test) = tcx.test(&test_name) else {
        return (StatusCode::NOT_FOUND, Html(format!("test `{}` not found", test_name)));
    };
    let Some(def) = tcx.definition(test.def_id) else {
        return (StatusCode::NOT_FOUND, Html(format!("definition `{:?}` not found", test.def_id)));
    };

    let evaluation_info = tcx.evaluation_info();

    let mut body = String::new();

    base_html(&mut body, &format!("test `{}`", def.def_path)).unwrap();
    write!(body, "<body>").unwrap();

    topbar_html(&mut body, wcx, &package, &target, tcx, None).unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    // NOTE: No sidebar, emit placeholder element.
    write!(body, "<div></div>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

    write!(body, "<header> ").unwrap();
    write!(body, "<h1>test <span class=\"inline-code\">{}</span></h1>", def.def_path_html).unwrap();
    if let Some(def_span) = &def.span {
        let Some(def_display_start_line) = def.display_start_line() else { unreachable!("definition with span has no display start line") };
        write!(body, " <span>in <a href=\"/{}/source/{}#L{}\">{}</a></span>", target_path, def_span.path.display(), def_display_start_line.0, def_span.path.display()).unwrap();
    }
    write!(body, "</header>").unwrap();

    let mut total_mutations_count = 0;
    let mut not_ran_mutations_count = 0;
    let mut undetected_mutations_count = 0;
    let mut detected_mutations_count = 0;
    let mut timed_out_mutations_count = 0;
    let mut crashed_mutations_count = 0;
    let test_runs = evaluation_info.and_then(|evaluation_info| evaluation_info.test_runs.get(&def.def_path));
    for &target_id in tcx.targets_reached_by_test(&def.def_path) {
        for &mutation_id in tcx.target_mutations(target_id) {
            match test_runs {
                Some(test_runs) => {
                    match test_runs.mutation_detections[mutation_id] {
                        None => continue,
                        Some(TestMutationResult::NotRan) => not_ran_mutations_count += 1,
                        Some(TestMutationResult::Ran(MutationDetection::Undetected)) => undetected_mutations_count += 1,
                        Some(TestMutationResult::Ran(MutationDetection::Detected)) => detected_mutations_count += 1,
                        Some(TestMutationResult::Ran(MutationDetection::TimedOut)) => timed_out_mutations_count += 1,
                        Some(TestMutationResult::Ran(MutationDetection::Crashed)) => crashed_mutations_count += 1,
                    }
                }
                None => not_ran_mutations_count += 1,
            }

            total_mutations_count += 1;
        }
    }

    write!(body, "<section class=\"text\">").unwrap();
    write!(body, "<h2>mutations</h2>").unwrap();
    match total_mutations_count {
        0 => {
            write!(body, "<p>no reachable mutations</p>").unwrap();
        }
        _ => {
            let ran_mutations_count = total_mutations_count - not_ran_mutations_count;
            let coverage = (ran_mutations_count - undetected_mutations_count) as f64 / ran_mutations_count as f64;

            write!(body, "<div class=\"labeled-stats\">").unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">reachable</div><div class=\"value\">{}</div></div>", total_mutations_count).unwrap();
            write!(body, "<div class=\"divider\"></div>").unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">ran</div><div class=\"value\">{}</div></div>", ran_mutations_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">skipped</div><div class=\"value\">{}</div></div>", not_ran_mutations_count).unwrap();
            write!(body, "<div class=\"divider\"></div>").unwrap();
            match ran_mutations_count {
                0 => write!(body, "<div class=\"stat\"><div class=\"label\">coverage</div><div class=\"value\">unknown</div></div>").unwrap(),
                _ => write!(body, "<div class=\"stat\"><div class=\"label\">coverage</div><div class=\"value\">{:.2}%</div></div>", coverage * 100_f64).unwrap(),
            }
            write!(body, "<div class=\"stat\"><div class=\"label\">undetected</div><div class=\"value\">{}</div></div>", undetected_mutations_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">detected</div><div class=\"value\">{}</div></div>", detected_mutations_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">timed out</div><div class=\"value\">{}</div></div>", timed_out_mutations_count).unwrap();
            write!(body, "<div class=\"stat\"><div class=\"label\">crashed</div><div class=\"value\">{}</div></div>", crashed_mutations_count).unwrap();
            write!(body, "</div>").unwrap();
        }
    }
    write!(body, "</section>").unwrap();

    let mut reachable_targets_per_file = BTreeMap::<PathBuf, Vec<TargetId>>::new();
    for &target_id in tcx.targets_reached_by_test(&def.def_path) {
        let Some(target) = tcx.target(target_id) else { continue; };
        let Some(def) = tcx.definition(target.def_id) else { continue; };
        let Some(def_span) = &def.span else { continue; };

        // NOTE: Skip targets for which no mutations were generated for.
        if tcx.target_mutations(target_id).is_empty() { continue; }

        let file_entry = reachable_targets_per_file.entry(def_span.path.to_owned()).or_default();
        if file_entry.contains(&target_id) { continue; }
        file_entry.push(target_id);
    }

    write!(body, "<section>").unwrap();
    write!(body, "<table class=\"page-table\">").unwrap();
    for (file_path, reachable_targets) in &reachable_targets_per_file {
        write!(body, "<tr class=\"heading\"><th colspan=\"3\">{}</th></tr>", file_path.display()).unwrap();

        for &mutation_id in tcx.file_mutations(file_path) {
            let Some(mutation) = tcx.mutation(mutation_id) else { continue; };

            if !reachable_targets.contains(&mutation.target_id) { continue; }

            let source_path_str = mutation.origin_span.path.to_string_lossy();
            let (lo_line, lo_col) = mutation.origin_span.begin;
            let (hi_line, hi_col) = mutation.origin_span.end;

            write!(body, "<tr id=\"M{}\" class=\"entry\">", mutation_id.0).unwrap();
            write!(body, "<td class=\"right\"><a href=\"/{}/source/{}#M{}\">{}:{} {}:{}</a></td>", target_path, source_path_str, mutation_id.0, lo_line, lo_col, hi_line, hi_col).unwrap();
            write!(body, "<td class=\"right right-tight\">").unwrap();
            if let Some(evaluation_info) = evaluation_info && let Some(test_mutation_result) = evaluation_info.test_mutation_result(mutation_id, &def.def_path) {
                write!(body, "{}", test_mutation_result.badge_html()).unwrap();
            }
            write!(body, "</td>").unwrap();
            write!(body, "<td class=\"expand\"><span><a href=\"/{}/mutations/{}\">mutation {}</a>: {}</span></td>", target_path, mutation_id.0, mutation_id.0, mutation.display_name_html).unwrap();
            write!(body, "</tr>").unwrap();
        }
    }
    write!(body, "</table>").unwrap();
    write!(body, "</section>").unwrap();

    write!(body, "</main>").unwrap();

    write!(body, "</div>").unwrap();

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}
