use std::fmt::Write;
use std::iter;
use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Redirect, Response};

use crate::config::Options;
use crate::ctxt::WebCtxt;
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

    let mut body = String::new();

    write!(body, "<!DOCTYPE html>").unwrap();
    write!(body, "<html lang=\"en\">").unwrap();
    write!(body, "<head>").unwrap();
    write!(body, "<title>{}</title>", path.display()).unwrap();
    write!(body, "<link rel=\"stylesheet\" href=\"/static/styles.css\">").unwrap();
    write!(body, "</head>").unwrap();
    write!(body, "<body>").unwrap();

    write!(body, "<nav class=\"sidebar\">").unwrap();
    write!(body, "<ol class=\"file-tree\">").unwrap();
    for tree_entry in file_tree_entries(wcx.local_source_file_paths()) {
        match tree_entry {
            TreeEntry::Dir(path) => {
                let Some(dir_name) = path.file_name() else {
                    return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid path amongst file tree entries: `{}`", path.display())));
                };
                write!(body, "<li><a class=\"item\">{}</a><ol>", dir_name.display()).unwrap();
            }
            TreeEntry::File(path) => {
                let Some(file_name) = path.file_name() else {
                    return (StatusCode::INTERNAL_SERVER_ERROR, Html(format!("encountered invalid path amongst file tree entries: `{}`", path.display())));
                };
                let file_mutations = wcx.file_mutations(path);
                write!(body, "<li><a class=\"item\" href=\"/source/{}\">{} <span class=\"badge\">{}</span></a></li>", path.display(), file_name.display(), file_mutations.len()).unwrap();
            }
            TreeEntry::EndDir => {
                write!(body, "</ol></li>").unwrap();
            }
        }
    }
    write!(body, "</ol>").unwrap();
    write!(body, "</nav>").unwrap();

    write!(body, "<main class=\"main-content\">").unwrap();

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
            let line_content = source_code_line_content(highlighted_line_html);
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
                    let line_content = match highlighted_line_html.is_empty() {
                        false => highlighted_line_html,
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

                // Write out mutation heading.
                write!(body, "<tbody data-group-id=\"{local_group_id}\" id=\"M{}\" class=\"mutation\"><tr class=\"heading\"><td colspan=\"2\">mutation {}: {}</td></tr>", mutation_id.0, mutation_id.0, mutation_html.display_name_html).unwrap();

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
                            let line_content = source_code_line_content(highlighted_line_html);
                            write!(body, "<tr class=\"line\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                        }
                    }
                    // Write out the original lines that this mutation subsitution modifies.
                    for (line_no, highlighted_line_html) in iter::zip(subst_start_line.0.., &subst_html.original_lines_html) {
                        let line_content = source_code_line_content(highlighted_line_html);
                        write!(body, "<tr class=\"line original\"><td class=\"line-no\">{}</td><td class=\"line-content\">{}</td></tr>", line_no, line_content).unwrap();
                    }
                    // Write out replacement lines.
                    for highlighted_line_html in &subst_html.replacement_lines_html {
                        let line_content = source_code_line_content(highlighted_line_html);
                        write!(body, "<tr class=\"line mutated\"><td></td><td class=\"line-content\">{}</td></tr>", line_content).unwrap();
                    }
                    // Write out suffix lines that are part of the group but are not modified by this particular mutation substitution.
                    if g.kind == OverlappingGroupKind::Replace {
                        let after_subst_start_line = LineNo(subst_end_line.0 + 1);
                        for (line_no, highlighted_line_html) in iter::zip(after_subst_start_line.0.., &source_file_html.highlighted_lines_html[after_subst_start_line..=g.end_line]) {
                            let line_content = source_code_line_content(highlighted_line_html);
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
            let line_content = match highlighted_line_html.is_empty() {
                false => highlighted_line_html,
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

    write!(body, "</body>").unwrap();
    write!(body, "</html>").unwrap();

    (StatusCode::OK, Html(body))
}
