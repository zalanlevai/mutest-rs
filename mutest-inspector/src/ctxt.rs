use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};

use mutest_json::DefId;
use mutest_json::data_structures::IdxVec;
use mutest_json::mutations::{MutationId, TargetId};

use crate::evaluation::EvaluationInfo;
use crate::html::{SourceFileHtml, render_def_path_html};
use crate::html::mutations::{OverlappingGroupOfMutations, update_overlapping_groups};
use crate::metadata::{Definition, Mutation, Target, Test};
use crate::source_file::SourceFile;

pub struct WebCtxt {
    unique_source_file_paths: Vec<PathBuf>,
    local_source_file_paths: Vec<PathBuf>,
    mutations_per_file: HashMap<PathBuf, Vec<MutationId>>,
    mutations_on_overlapping_lines: HashMap<PathBuf, Vec<OverlappingGroupOfMutations>>,
    loaded_source_files: HashMap<PathBuf, SourceFile>,
    source_file_htmls: HashMap<PathBuf, SourceFileHtml>,
    definitions: IdxVec<DefId, Definition>,
    tests: BTreeMap<String, Test>,
    targets: IdxVec<TargetId, Target>,
    mutations: IdxVec<MutationId, Mutation>,
    evaluation_info: Option<EvaluationInfo>,
}

impl WebCtxt {
    pub fn new(
        file_paths: &HashSet<&Path>,
        call_graph_metadata: &mutest_json::call_graph::CallGraphInfo,
        mutations_metadata: &mutest_json::mutations::MutationsInfo,
    ) -> Self {
        let mut unique_source_file_paths = file_paths.into_iter().map(|&p| p.to_owned()).collect::<Vec<_>>();
        unique_source_file_paths.sort();

        let local_source_file_paths = unique_source_file_paths.iter()
            .filter(|path| path.is_relative())
            .cloned()
            .collect::<Vec<_>>();

        let mut mutations_per_file = HashMap::<PathBuf, Vec<MutationId>>::new();
        for mutation in &mutations_metadata.mutations {
            for subst in &mutation.substs {
                let file_entry = mutations_per_file.entry(subst.location.span().path.to_owned()).or_default();
                if file_entry.contains(&mutation.mutation_id) { continue; }
                // NOTE: The insertions in this loop guarantee that each entry's mutation ID list is ordered.
                file_entry.push(mutation.mutation_id);
            }
        }

        let mut mutations_on_overlapping_lines = HashMap::<PathBuf, Vec<OverlappingGroupOfMutations>>::new();
        for mutation in &mutations_metadata.mutations {
            update_overlapping_groups(&mut mutations_on_overlapping_lines, mutation);
        }

        let mut definitions = IdxVec::with_capacity(call_graph_metadata.definitions.len());
        for definition in &call_graph_metadata.definitions {
            definitions.push(Definition {
                def_id: definitions.next_index(),
                name: definition.name.clone(),
                def_path: definition.path.clone(),
                def_path_html: render_def_path_html(&definition.path),
                span: definition.span.clone(),
            });
        }

        let mut targets = IdxVec::with_capacity(mutations_metadata.targets.len());
        for target in &mutations_metadata.targets {
            targets.push(Target {
                def_id: target.def_id,
                reachable_from: target.reachable_from.iter()
                    .map(|(def_path, _association)| (def_path.clone(), ()))
                    .collect(),
            });
        }

        let mut tests = BTreeMap::new();
        for entry_point in &call_graph_metadata.call_graph.entry_points {
            let def = &call_graph_metadata.definitions[entry_point.def_id];

            tests.insert(def.path.clone(), Test {
                def_id: entry_point.def_id,
                ignore: false,
                span: def.span.clone().expect("encountered test without span"),
            });
        }

        Self {
            unique_source_file_paths,
            local_source_file_paths,
            mutations_per_file,
            mutations_on_overlapping_lines,
            loaded_source_files: Default::default(),
            source_file_htmls: Default::default(),
            definitions,
            tests,
            targets,
            mutations: IdxVec::new(),
            evaluation_info: None,
        }
    }

    pub(crate) fn register_loaded_source_file(&mut self, file_path: &Path, source_file: SourceFile, source_file_html: SourceFileHtml) {
        self.loaded_source_files.insert(file_path.to_owned(), source_file);
        self.source_file_htmls.insert(file_path.to_owned(), source_file_html);
    }

    pub(crate) fn register_loaded_mutations(&mut self, mutations: IdxVec<MutationId, Mutation>) {
        self.mutations = mutations;
    }

    pub(crate) fn update_evaluation_info(&mut self, evaluation_info: Option<EvaluationInfo>) {
        self.evaluation_info = evaluation_info;
    }

    #[inline]
    pub fn unique_source_file_paths(&self) -> &[PathBuf] {
        &self.unique_source_file_paths
    }

    #[inline]
    pub fn local_source_file_paths(&self) -> &[PathBuf] {
        &self.local_source_file_paths
    }

    #[inline]
    pub fn file_mutations(&self, path: &Path) -> &[MutationId] {
        self.mutations_per_file.get(path).map(|vec| -> &[_] { vec }).unwrap_or_default()
    }

    #[inline]
    pub fn loaded_source_file(&self, path: &Path) -> Option<&SourceFile> {
        self.loaded_source_files.get(path)
    }

    #[inline]
    pub fn source_file_html(&self, path: &Path) -> Option<&SourceFileHtml> {
        self.source_file_htmls.get(path)
    }

    #[inline]
    pub fn definition(&self, def_id: DefId) -> Option<&Definition> {
        self.definitions.get(def_id)
    }

    #[inline]
    pub fn lookup_definition_by_path(&self, def_path: &str) -> Option<&Definition> {
        self.definitions.iter().find(|definition| definition.def_path == def_path)
    }

    #[inline]
    pub fn tests(&self) -> impl Iterator<Item = &Test> {
        self.tests.values()
    }

    #[inline]
    pub fn tests_count(&self) -> usize {
        self.tests.len()
    }

    #[inline]
    pub fn test(&self, def_path: &str) -> Option<&Test> {
        self.tests.get(def_path)
    }

    #[inline]
    pub fn target(&self, target_id: TargetId) -> Option<&Target> {
        self.targets.get(target_id)
    }

    #[inline]
    pub fn mutations_count(&self) -> usize {
        self.mutations.len()
    }

    #[inline]
    pub fn mutation(&self, mutation_id: MutationId) -> Option<&Mutation> {
        self.mutations.get(mutation_id)
    }

    #[inline]
    pub fn overlapping_groups_of_mutations_in_file(&self, path: &Path) -> &[OverlappingGroupOfMutations] {
        self.mutations_on_overlapping_lines.get(path).map(|vec| -> &[_] { vec }).unwrap_or_default()
    }

    #[inline]
    pub fn evaluation_info(&self) -> Option<&EvaluationInfo> {
        self.evaluation_info.as_ref()
    }
}
