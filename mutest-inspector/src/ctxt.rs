use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use mutest_json::data_structures::{IdxVec, IdxSlice};
use mutest_json::mutations::MutationId;

use crate::html::SourceFileHtml;
use crate::html::mutations::{MutationHtml, OverlappingGroupOfMutations, update_overlapping_groups};
use crate::source_file::SourceFile;

pub struct WebCtxt {
    unique_source_file_paths: Vec<PathBuf>,
    local_source_file_paths: Vec<PathBuf>,
    mutations_per_file: HashMap<PathBuf, Vec<MutationId>>,
    mutations_on_overlapping_lines: HashMap<PathBuf, Vec<OverlappingGroupOfMutations>>,
    loaded_source_files: HashMap<PathBuf, SourceFile>,
    source_file_htmls: HashMap<PathBuf, SourceFileHtml>,
    mutation_htmls: IdxVec<MutationId, MutationHtml>,
}

impl WebCtxt {
    pub fn new(file_paths: &HashSet<&Path>, mutations: &IdxSlice<MutationId, mutest_json::mutations::Mutation>) -> Self {
        let mut unique_source_file_paths = file_paths.into_iter().map(|&p| p.to_owned()).collect::<Vec<_>>();
        unique_source_file_paths.sort();

        let local_source_file_paths = unique_source_file_paths.iter()
            .filter(|path| path.is_relative())
            .cloned()
            .collect::<Vec<_>>();

        let mut mutations_per_file = HashMap::<PathBuf, Vec<MutationId>>::new();
        for mutation in mutations {
            for subst in &mutation.substs {
                let file_entry = mutations_per_file.entry(subst.location.span().path.to_owned()).or_default();
                if file_entry.contains(&mutation.mutation_id) { continue; }
                // NOTE: The insertions in this loop guarantee that each entry's mutation ID list is ordered.
                file_entry.push(mutation.mutation_id);
            }
        }

        let mut mutations_on_overlapping_lines = HashMap::<PathBuf, Vec<OverlappingGroupOfMutations>>::new();
        for mutation in mutations {
            update_overlapping_groups(&mut mutations_on_overlapping_lines, mutation);
        }

        Self {
            unique_source_file_paths,
            local_source_file_paths,
            mutations_per_file,
            mutations_on_overlapping_lines,
            loaded_source_files: Default::default(),
            source_file_htmls: Default::default(),
            mutation_htmls: IdxVec::new(),
        }
    }

    pub(crate) fn register_loaded_source_file(&mut self, file_path: &Path, source_file: SourceFile, source_file_html: SourceFileHtml) {
        self.loaded_source_files.insert(file_path.to_owned(), source_file);
        self.source_file_htmls.insert(file_path.to_owned(), source_file_html);
    }

    pub(crate) fn register_loaded_mutations(&mut self, mutation_htmls: IdxVec<MutationId, MutationHtml>) {
        self.mutation_htmls = mutation_htmls;
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
    pub fn mutation_html(&self, mutation_id: MutationId) -> Option<&MutationHtml> {
        self.mutation_htmls.get(mutation_id)
    }

    #[inline]
    pub fn overlapping_groups_of_mutations_in_file(&self, path: &Path) -> &[OverlappingGroupOfMutations] {
        self.mutations_on_overlapping_lines.get(path).map(|vec| -> &[_] { vec }).unwrap_or_default()
    }
}
