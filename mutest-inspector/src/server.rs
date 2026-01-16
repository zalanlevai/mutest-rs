use std::fmt::Write;
use std::iter;
use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Redirect, Response};

use crate::config::Options;
use crate::ctxt::WebCtxt;
use crate::evaluation::MutationDetection;
use crate::html::source_code_line_content;
use crate::html::mutations::{OverlappingGroupOfMutations, OverlappingGroupKind};
use crate::source_file::{LineNo, TreeEntry, file_tree_entries};

pub(crate) struct ServerState {
    pub wcx: WebCtxt,
}

pub(crate) async fn run(opts: &Options, state: ServerState) {
    let state = Arc::new(state);

    let router = axum::Router::new()
        .route("/", axum::routing::get(handle_root))
        .route("/source/{*path}", axum::routing::get(handle_source_request))
        .nest_service("/static", tower_http::services::ServeDir::new(env!("STATIC_DIR")))
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

async fn handle_root() -> Response {
    // FIXME: Look up actual root source file.
    Redirect::to("/source/src/lib.rs").into_response()
}

async fn handle_source_request(State(state): State<Arc<ServerState>>, Path(path): Path<PathBuf>) -> (StatusCode, Html<String>) {
    let wcx = &state.wcx;

    let Some(source_file_html) = wcx.source_file_html(&path) else {
        return (StatusCode::NOT_FOUND, Html(format!("source file `{}` not found", path.display())));
    };

    let overlapping_groups_of_mutations_in_file = wcx.overlapping_groups_of_mutations_in_file(&path);

    let evaluation_info = wcx.evaluation_info();

    let mut total_mutations_count = 0;
    let mut undetected_mutations_count = 0;
    let mut detected_mutations_count = 0;
    let mut timed_out_mutations_count = 0;
    let mut crashed_mutations_count = 0;
    if let Some(evaluation_info) = evaluation_info {
        for group in overlapping_groups_of_mutations_in_file {
            for &mutation_id in &group.mutations {
                total_mutations_count += 1;

                let mutation_detection = evaluation_info.mutation_detections[mutation_id];
                match mutation_detection {
                    MutationDetection::Undetected => undetected_mutations_count += 1,
                    MutationDetection::Detected => detected_mutations_count += 1,
                    MutationDetection::TimedOut => timed_out_mutations_count += 1,
                    MutationDetection::Crashed => crashed_mutations_count += 1,
                }
            }
        }
    }

    let mut body = String::new();

    write!(body, "<!DOCTYPE html>").unwrap();
    write!(body, "<html lang=\"en\">").unwrap();
    write!(body, "<head>").unwrap();
    write!(body, "<title>{}</title>", path.display()).unwrap();
    write!(body, "<link rel=\"stylesheet\" href=\"/static/styles.css\">").unwrap();
    write!(body, "</head>").unwrap();
    write!(body, "<body>").unwrap();

    write!(body, "<nav class=\"topbar\">").unwrap();
    write!(body, "<a class=\"logo\" href=\"/\">mutest-rs</a>").unwrap();
    write!(body, "<a class=\"tab active\" href=\"/source\">sources</a>").unwrap();
    write!(body, "</nav>").unwrap();

    write!(body, "<div class=\"page-layout\">").unwrap();

    write!(body, "<nav class=\"sidebar\">").unwrap();
    write!(body, "<ol class=\"file-tree\">").unwrap();
    for tree_entry in file_tree_entries(wcx.local_source_file_paths()) {
        match tree_entry {
            TreeEntry::Dir(entry_path) => {
                let Some(dir_name) = entry_path.file_name() else {
                    return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid path amongst file tree entries: `{}`", entry_path.display())));
                };
                write!(body, "<li><a class=\"item\"><span class=\"name\">{}</span></a><ol>", dir_name.display()).unwrap();
            }
            TreeEntry::File(entry_path) => {
                let Some(file_name) = entry_path.file_name() else {
                    return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid path amongst file tree entries: `{}`", entry_path.display())));
                };
                let file_mutations = wcx.file_mutations(entry_path);

                write!(body, "<li><a class=\"item").unwrap();
                if entry_path == path {
                    write!(body, " selected").unwrap();
                }
                write!(body, "\" href=\"/source/{}\">", entry_path.display()).unwrap();
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

    write!(body, "<header>").unwrap();
    write!(body, "<h1>{}</h1>", path.display()).unwrap();
    if let Some(_evaluation_info) = evaluation_info {
        match total_mutations_count {
            0 => {
                write!(body, "<div class=\"stats none\">").unwrap();
                write!(body, "<span class=\"label\">no mutations</span>").unwrap();
                write!(body, "</div>").unwrap();
            }
            _ => {
                let mutation_score = (total_mutations_count - undetected_mutations_count) as f64 / total_mutations_count as f64;
                write!(body, "<div class=\"stats\">").unwrap();
                write!(body, "<span class=\"value total\">{}</span><span class=\"label\">mutations</span>", total_mutations_count).unwrap();
                write!(body, "<span class=\"value score\">{:.2}%</span><span class=\"label\">coverage</span>", mutation_score * 100_f64).unwrap();
                write!(body, "<span class=\"value undetected\">{}</span><span class=\"label\">undetected</span>", undetected_mutations_count).unwrap();
                write!(body, "<span class=\"value detected\">{}</span><span class=\"label\">detected</span>", detected_mutations_count).unwrap();
                write!(body, "<span class=\"value timed-out\">{}</span><span class=\"label\">timed out</span>", timed_out_mutations_count).unwrap();
                write!(body, "<span class=\"value crashed\">{}</span><span class=\"label\">crashed</span>", crashed_mutations_count).unwrap();
                write!(body, "</div>").unwrap();
            }
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
                let Some(mutation_html) = wcx.mutation_html(mutation_id) else { continue; };

                let mutation_detection_html = match evaluation_info {
                    Some(evaluation_info) => evaluation_info.mutation_detections[mutation_id].badge_html(),
                    None => "",
                };

                // Write out mutation heading.
                write!(body, "<tbody data-group-id=\"{local_group_id}\" id=\"M{}\" class=\"mutation\"><tr class=\"heading\"><td colspan=\"2\">{}mutation {}: {}</td></tr>", mutation_id.0, mutation_detection_html, mutation_id.0, mutation_html.display_name_html).unwrap();

                // Filter to relevant substitutions that fall within this overlap group.
                let substs_within_group = mutation_html.subst_htmls.iter().filter(|subst_html| {
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
