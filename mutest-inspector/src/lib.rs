#![feature(array_windows)]
#![feature(if_let_guard)]

pub mod call_graph;
pub mod config;
pub mod ctxt;
pub mod evaluation;
pub mod html;
pub mod metadata;
pub mod server;
pub mod source_file;
pub mod syntax_highlight;

use std::collections::{HashMap, HashSet};
use std::time::Instant;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use mutest_json::data_structures::IdxVec;
use mutest_json::mutations::MutationId;

use crate::config::Config;
use crate::ctxt::WebCtxt;
use crate::evaluation::{EvaluationInfo, MutationDetection, TestMutationResult, TestRuns};
use crate::html::{SourceFileHtml, escape_html_body_text_with_inline_code};
use crate::html::mutations::render_mutation_subst_lines;
use crate::metadata::Mutation;
use crate::source_file::SourceFile;
use crate::syntax_highlight::SyntaxHighlighter;

pub async fn run(config: Config) {
    let t_start = Instant::now();

    let call_graph_metadata_file_str = fs::read_to_string(config.json_dir_path.join("call_graph.json")).expect("cannot read call graph metadata file");
    let call_graph_metadata = serde_json::from_str::<mutest_json::call_graph::CallGraphInfo>(&call_graph_metadata_file_str).expect("cannot parse call graph metadata file");

    let mutations_metadata_file_str = fs::read_to_string(config.json_dir_path.join("mutations.json")).expect("cannot read mutations metadata file");
    let mutations_metadata = serde_json::from_str::<mutest_json::mutations::MutationsInfo>(&mutations_metadata_file_str).expect("cannot parse mutations metadata file");

    let evaluation_metadata_file_str = match fs::read_to_string(config.json_dir_path.join("evaluation.json")){
        Ok(v) => Some(v),
        Err(err) if matches!(err.kind(), io::ErrorKind::NotFound) => None,
        Err(err) => panic!("cannot read evaluation metadata file: {err}"),
    };
    let evaluation_metadata = evaluation_metadata_file_str.as_ref().map(|evaluation_metadata_file_str| {
        serde_json::from_str::<mutest_json::evaluation::EvaluationInfo>(evaluation_metadata_file_str).expect("cannot parse evaluation metadata file")
    });

    let unique_source_file_paths = call_graph_metadata.definitions.iter()
        .filter_map(|def| def.span.as_ref().map(|span| -> &Path { &span.path }))
        .collect::<HashSet<_>>();

    let mut wcx = WebCtxt::new(&unique_source_file_paths, &call_graph_metadata, &mutations_metadata);

    println!("processing {} source files", wcx.unique_source_file_paths().len());

    let mut syntax_highlighter = SyntaxHighlighter::init();

    // NOTE: Ensure that we attempt to load source files in a deterministic order.
    let mut unique_source_file_paths = unique_source_file_paths.into_iter().map(|p| p.to_owned()).collect::<Vec<_>>();
    unique_source_file_paths.sort();
    for source_file_path in &unique_source_file_paths {
        let Ok(source) = fs::read_to_string(source_file_path) else { continue; };

        println!("processing {}", source_file_path.display());

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
        let mut mutation_html = Mutation {
            target_id: mutation.target_id,
            origin_span: mutation.origin_span.clone(),
            mutation_op: mutation.mutation_op.clone(),
            display_name: mutation.display_name.clone(),
            display_name_html: escape_html_body_text_with_inline_code(&mutation.display_name),
            subst_htmls: Vec::with_capacity(mutation.substs.len()),
            safety: mutation.safety,
        };

        for subst in &mutation.substs {
            let Some(subst_html) = render_mutation_subst_lines(&wcx, &mut syntax_highlighter, subst) else { continue; };
            mutation_html.subst_htmls.push(subst_html);
        }

        // NOTE: Mutation HTML data is populated in mutation ID order.
        mutation_htmls.push(mutation_html);
    }
    wcx.register_loaded_mutations(mutation_htmls);

    if let Some(evaluation_metadata) = &evaluation_metadata {
        let mut evaluation_info = EvaluationInfo {
            mutation_detections: IdxVec::with_capacity(mutations_metadata.mutations.len()),
            test_runs: HashMap::with_capacity(wcx.tests_count()),
        };

        let mutation_run = match &evaluation_metadata.mutation_runs[..] {
            [] => panic!("evaluation metadata contains no mutation runs"),
            [mutation_run] => mutation_run,
            // TODO: Warn user that we are ignoring all subsequent mutation runs.
            [first_mutation_run, ..] => first_mutation_run,
        };

        for mutation_detection in &mutation_run.mutation_detection_matrix.overall_detections.0 {
            let mutation_detection = match mutation_detection {
                mutest_json::evaluation::MutationDetection::NotRun => panic!("encountered unexpected mutation detection value"),
                mutest_json::evaluation::MutationDetection::Undetected => MutationDetection::Undetected,
                mutest_json::evaluation::MutationDetection::Detected => MutationDetection::Detected,
                mutest_json::evaluation::MutationDetection::TimedOut => MutationDetection::TimedOut,
                mutest_json::evaluation::MutationDetection::Crashed => MutationDetection::Crashed,
            };

            // NOTE: Mutation detection data is populated in mutation ID order.
            evaluation_info.mutation_detections.push(mutation_detection);
        }

        for (runtime_test_id, mutation_detections) in mutation_run.mutation_detection_matrix.test_detections.iter_enumerated() {
            let test_path = &evaluation_metadata.tests[runtime_test_id].name;

            let reachable_targets = mutations_metadata.targets.iter()
                .filter(|target| target.reachable_from.contains_key(test_path))
                .map(|target| target.target_id)
                .collect::<HashSet<_>>();

            let mut test_runs = TestRuns {
                mutation_detections: IdxVec::with_capacity(mutations_metadata.mutations.len()),
            };

            for (mutation_id, mutation_detection) in mutation_detections.0.iter_enumerated() {
                let mutation = &mutations_metadata.mutations[mutation_id];

                let test_mutation_result = match mutation_detection {
                    mutest_json::evaluation::MutationDetection::NotRun if !reachable_targets.contains(&mutation.target_id) => None,
                    mutest_json::evaluation::MutationDetection::NotRun => Some(TestMutationResult::NotRan),
                    mutest_json::evaluation::MutationDetection::Undetected => Some(TestMutationResult::Ran(MutationDetection::Undetected)),
                    mutest_json::evaluation::MutationDetection::Detected => Some(TestMutationResult::Ran(MutationDetection::Detected)),
                    mutest_json::evaluation::MutationDetection::TimedOut => Some(TestMutationResult::Ran(MutationDetection::TimedOut)),
                    mutest_json::evaluation::MutationDetection::Crashed => Some(TestMutationResult::Ran(MutationDetection::Crashed)),
                };

                // NOTE: Mutation detection data is populated in mutation ID order.
                test_runs.mutation_detections.push(test_mutation_result);
            }

            evaluation_info.test_runs.insert(test_path.clone(), test_runs);
        }

        wcx.update_evaluation_info(Some(evaluation_info));
    }

    println!("finished in {:.2?}", t_start.elapsed());

    let state = server::ServerState {
        wcx,
    };

    server::run(&config.opts, state).await;
}
