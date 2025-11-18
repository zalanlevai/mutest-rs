pub mod config;
pub mod ctxt;
pub mod html;
pub mod server;
pub mod source_file;
pub mod syntax_highlight;

use std::collections::{HashMap, HashSet};
use std::time::Instant;
use std::fs;
use std::path::{Path, PathBuf};

use mutest_json::data_structures::IdxVec;
use mutest_json::mutations::MutationId;

use crate::config::Config;
use crate::ctxt::WebCtxt;
use crate::html::{SourceFileHtml, escape_html_body_text_with_inline_code};
use crate::html::mutations::{MutationHtml, render_mutation_subst_lines};
use crate::source_file::SourceFile;
use crate::syntax_highlight::SyntaxHighlighter;

pub async fn run(config: Config) {
    let t_start = Instant::now();

    let call_graph_metadata_file_str = fs::read_to_string(config.json_dir_path.join("call_graph.json")).expect("cannot read call graph metadata file");
    let call_graph_metadata = serde_json::from_str::<mutest_json::call_graph::CallGraphInfo>(&call_graph_metadata_file_str).expect("cannot parse call graph metadata file");

    let mutations_metadata_file_str = fs::read_to_string(config.json_dir_path.join("mutations.json")).expect("cannot read mutations metadata file");
    let mutations_metadata = serde_json::from_str::<mutest_json::mutations::MutationsInfo>(&mutations_metadata_file_str).expect("cannot parse mutations metadata file");

    let unique_source_file_paths = call_graph_metadata.definitions.iter()
        .filter_map(|def| def.span.as_ref().map(|span| -> &Path { &span.path }))
        .collect::<HashSet<_>>();

    let mut wcx = WebCtxt::new(&unique_source_file_paths, &mutations_metadata.mutations);

    println!("processing {} source files", wcx.unique_source_file_paths().len());

    let mut syntax_highlighter = SyntaxHighlighter::init();

    // NOTE: Ensure that we attempt to load source files in a deterministic order.
    let mut unique_source_file_paths = unique_source_file_paths.into_iter().map(|p| p.to_owned()).collect::<Vec<_>>();
    unique_source_file_paths.sort();
    for source_file_path in &unique_source_file_paths {
        let Ok(source) = fs::read_to_string(source_file_path) else { continue; };

        let mut source_file_lines = IdxVec::new();
        for line in source.lines() {
            source_file_lines.push(line.to_owned());
        }
        // NOTE: The `str::lines()` iterator ignores trailing newlines, which we want to keep.
        if source.ends_with("\n") || source.ends_with("\r\n") {
            source_file_lines.push(String::new());
        }

        let source_file = SourceFile {
            lines: source_file_lines,
        };

        let highlighted_lines_html = syntax_highlighter.highlight_lines_html(&source)
            .expect(&format!("cannot syntax highlight source file `{}`", source_file_path.display()));

        // HACK: Remove this, figure out a better API on Idx* to perform such an operation.
        let highlighted_lines_html = {
            let mut idx_vec = IdxVec::with_capacity(highlighted_lines_html.len());
            for highlighted_line_html in highlighted_lines_html {
                idx_vec.push(highlighted_line_html);
            }
            idx_vec
        };

        let source_file_html = SourceFileHtml {
            display_path_html: format!("{}", source_file_path.display()),
            highlighted_lines_html,
        };

        wcx.register_loaded_source_file(source_file_path, source_file, source_file_html);
    }

    let mut mutations_per_file = HashMap::<PathBuf, Vec<MutationId>>::new();
    for mutation in &mutations_metadata.mutations {
        for subst in &mutation.substs {
            let file_entry = mutations_per_file.entry(subst.location.span().path.to_owned()).or_default();
            if file_entry.contains(&mutation.mutation_id) { continue; }
            // NOTE: The insertions in this loop guarantee that each entry's mutation ID list is ordered.
            file_entry.push(mutation.mutation_id);
        }
    }

    println!("processing {} mutations", mutations_metadata.mutations.len());

    let mut mutation_htmls = IdxVec::<MutationId, _>::with_capacity(mutations_metadata.mutations.len());
    for mutation in &mutations_metadata.mutations {
        let mut mutation_html = MutationHtml {
            display_name_html: escape_html_body_text_with_inline_code(&mutation.display_name),
            subst_htmls: Vec::with_capacity(mutation.substs.len()),
        };

        for subst in &mutation.substs {
            let Some(subst_html) = render_mutation_subst_lines(&wcx, &mut syntax_highlighter, subst) else { continue; };
            mutation_html.subst_htmls.push(subst_html);
        }

        // NOTE: Mutation HTML data is populated in mutation ID order.
        mutation_htmls.push(mutation_html);
    }
    wcx.register_loaded_mutations(mutation_htmls);

    println!("finished in {:.2?}", t_start.elapsed());

    let state = server::ServerState {
        wcx,
    };

    server::run(&config.opts, state).await;
}
