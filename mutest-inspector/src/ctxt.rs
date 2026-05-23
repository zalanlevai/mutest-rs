use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};

use mutest_json::DefId;
use mutest_json::call_graph::CallGraph;
use mutest_json::data_structures::IdxVec;
use mutest_json::mutations::{MutationId, TargetId};
use serde::Deserialize;

use crate::evaluation::EvaluationInfo;
use crate::html::{SourceFileHtml, render_def_path_html};
use crate::html::mutations::{OverlappingGroupOfMutations, update_overlapping_groups};
use crate::metadata::{Definition, Mutation, Target, Test};
use crate::source_file::{SourceFile, nudge_span_prefix_lines};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum TargetSpec {
    Lib,
    MainBin,
    Bin(String),
    Example(String),
    Test(String),
}

impl TargetSpec {
    pub fn from_path_str(string: &str) -> Result<Self, ()> {
        match string {
            "lib" => Ok(TargetSpec::Lib),
            "bin" => Ok(TargetSpec::MainBin),
            _ if let Some(name) = string.strip_prefix("bin:") => Ok(TargetSpec::Bin(name.to_owned())),
            _ if let Some(name) = string.strip_prefix("example:") => Ok(TargetSpec::Example(name.to_owned())),
            _ if let Some(name) = string.strip_prefix("test:") => Ok(TargetSpec::Test(name.to_owned())),
            _ => Err(()),
        }
    }

    pub fn path_str(&self) -> String {
        match self {
            TargetSpec::Lib => "lib".to_owned(),
            TargetSpec::MainBin => "bin".to_owned(),
            TargetSpec::Bin(name) => format!("bin:{}", name),
            TargetSpec::Example(name) => format!("example:{}", name),
            TargetSpec::Test(name) => format!("test:{}", name),
        }
    }
}

impl<'de> Deserialize<'de> for TargetSpec {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let string = <&str>::deserialize(deserializer)?;
        TargetSpec::from_path_str(string).map_err(|_| D::Error::custom(format!("invalid target selector `{}`", string)))
    }
}

pub fn workspace_target_display_str(package: &str, target: &TargetSpec) -> String {
    match target {
        TargetSpec::Lib => format!("{} (lib)", package),
        TargetSpec::MainBin => format!("{} (bin)", package),
        TargetSpec::Bin(name) => format!("{}/{} (bin)", package, name),
        TargetSpec::Example(name) => format!("{}/{} (example)", package, name),
        TargetSpec::Test(name) => format!("{}/{} (test)", package, name),
    }
}

pub struct PackageCtxt {
    pub lib: Option<TargetCtxt>,
    pub bin: Option<TargetCtxt>,
    pub bins: HashMap<String, TargetCtxt>,
    pub examples: HashMap<String, TargetCtxt>,
    pub tests: HashMap<String, TargetCtxt>,
}

impl PackageCtxt {
    pub fn empty() -> Self {
        Self {
            lib: None,
            bin: None,
            bins: Default::default(),
            examples: Default::default(),
            tests: Default::default(),
        }
    }

    pub fn target(&self, target: &TargetSpec) -> Option<&TargetCtxt> {
        match target {
            TargetSpec::Lib => self.lib.as_ref(),
            TargetSpec::MainBin => self.bin.as_ref(),
            TargetSpec::Bin(name) => self.bins.get(name),
            TargetSpec::Example(name) => self.examples.get(name),
            TargetSpec::Test(name) => self.tests.get(name),
        }
    }

    pub fn iter_targets(&self) -> impl Iterator<Item = (TargetSpec, &TargetCtxt)> {
        self.lib.as_ref().map(|tcx| (TargetSpec::Lib, tcx)).into_iter()
            .chain(self.bin.as_ref().map(|tcx| (TargetSpec::MainBin, tcx)))
            .chain(self.bins.iter().map(|(name, tcx)| (TargetSpec::Bin(name.to_owned()), tcx)))
            .chain(self.examples.iter().map(|(name, tcx)| (TargetSpec::Example(name.to_owned()), tcx)))
            .chain(self.tests.iter().map(|(name, tcx)| (TargetSpec::Test(name.to_owned()), tcx)))
    }
}

pub struct WorkspaceCtxt {
    packages: HashMap<String, PackageCtxt>,
    loaded_targets: Vec<(String, TargetSpec)>,
    loaded_source_files: HashMap<PathBuf, SourceFile>,
    source_file_htmls: HashMap<PathBuf, SourceFileHtml>,
}

impl WorkspaceCtxt {
    pub fn empty() -> Self {
        Self {
            packages: Default::default(),
            loaded_targets: Vec::new(),
            loaded_source_files: Default::default(),
            source_file_htmls: Default::default(),
        }
    }

    pub(crate) fn register_target(&mut self, package: String, target: TargetSpec, tcx: TargetCtxt) {
        let target_entry = (package.clone(), target.clone());

        let pcx = self.packages.entry(package).or_insert_with(PackageCtxt::empty);
        match target {
            TargetSpec::Lib => { pcx.lib = Some(tcx); }
            TargetSpec::MainBin => { pcx.bin = Some(tcx); }
            TargetSpec::Bin(name) => { pcx.bins.insert(name, tcx); }
            TargetSpec::Example(name) => { pcx.examples.insert(name, tcx); }
            TargetSpec::Test(name) => { pcx.tests.insert(name, tcx); }
        }

        // NOTE: Ensure that the targets list is always sorted by always using sorted insertions.
        match self.loaded_targets.binary_search(&target_entry) {
            Ok(_) => {}
            Err(i) => self.loaded_targets.insert(i, target_entry),
        };
    }

    pub(crate) fn register_loaded_source_file(&mut self, file_path: &Path, source_file: SourceFile, source_file_html: SourceFileHtml) {
        self.loaded_source_files.insert(file_path.to_owned(), source_file);
        self.source_file_htmls.insert(file_path.to_owned(), source_file_html);
    }

    #[inline]
    pub fn targets(&self) -> &[(String, TargetSpec)] {
        &self.loaded_targets
    }

    #[inline]
    pub fn target(&self, package: &str, target: &TargetSpec) -> Option<&TargetCtxt> {
        self.packages.get(package).and_then(|pcx| pcx.target(target))
    }

    #[inline]
    pub fn loaded_source_file(&self, path: &Path) -> Option<&SourceFile> {
        self.loaded_source_files.get(path)
    }

    #[inline]
    pub fn source_file_html(&self, path: &Path) -> Option<&SourceFileHtml> {
        self.source_file_htmls.get(path)
    }
}

pub struct TargetCtxt {
    unique_source_file_paths: Vec<PathBuf>,
    local_source_file_paths: Vec<PathBuf>,
    mutations_per_file: HashMap<PathBuf, Vec<MutationId>>,
    mutations_on_overlapping_lines: HashMap<PathBuf, Vec<OverlappingGroupOfMutations>>,
    definitions: IdxVec<DefId, Definition>,
    tests: BTreeMap<String, Test>,
    call_graph: CallGraph,
    targets: IdxVec<TargetId, Target>,
    targets_reached_by_tests: HashMap<String, Vec<TargetId>>,
    mutations_per_target: IdxVec<TargetId, Vec<MutationId>>,
    mutations: IdxVec<MutationId, Mutation>,
    evaluation_info: Option<EvaluationInfo>,
}

impl TargetCtxt {
    pub fn new(
        wcx: &WorkspaceCtxt,
        file_paths: HashSet<PathBuf>,
        call_graph_metadata: &mutest_json::call_graph::CallGraphInfo,
        mutations_metadata: &mutest_json::mutations::MutationsInfo,
    ) -> Self {
        let mut unique_source_file_paths = file_paths.into_iter().collect::<Vec<_>>();
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
            let mut nudge_prefix_lines = 0;
            if let Some(span) = &definition.span && let Some(source_file) = wcx.loaded_source_file(&span.path) {
                nudge_prefix_lines = nudge_span_prefix_lines(&source_file.lines, span);
            }

            definitions.push(Definition {
                def_id: definitions.next_index(),
                name: definition.name.clone(),
                def_path: definition.path.clone(),
                def_path_html: render_def_path_html(&definition.path),
                span: definition.span.clone(),
                nudge_prefix_lines,
            });
        }

        let mut targets = IdxVec::with_capacity(mutations_metadata.targets.len());
        let mut mutations_per_target = IdxVec::with_capacity(mutations_metadata.targets.len());
        for target in &mutations_metadata.targets {
            targets.push(Target {
                def_id: target.def_id,
                reachable_from: target.reachable_from.iter()
                    .map(|(def_path, _association)| (def_path.clone(), ()))
                    .collect(),
            });

            let mutations = mutations_metadata.mutations.iter()
                .filter(|mutation| mutation.target_id == target.target_id)
                .map(|mutation| mutation.mutation_id)
                .collect::<Vec<_>>();
            // NOTE: Data is populated in target ID order.
            mutations_per_target.push(mutations);
        }

        let mut tests = BTreeMap::new();
        let mut targets_reached_by_tests = HashMap::with_capacity(call_graph_metadata.call_graph.entry_points.len());
        for entry_point in &call_graph_metadata.call_graph.entry_points {
            let def = &call_graph_metadata.definitions[entry_point.def_id];

            tests.insert(def.path.clone(), Test {
                def_id: entry_point.def_id,
                ignore: false,
                span: def.span.clone().expect("encountered test without span"),
            });

            let reachable_targets = mutations_metadata.targets.iter()
                .filter(|target| target.reachable_from.contains_key(&def.path))
                .map(|target| target.target_id)
                .collect::<HashSet<_>>();
            let mut reachable_targets = reachable_targets.into_iter().collect::<Vec<_>>();
            reachable_targets.sort_unstable_by_key(|target_id| target_id.0);
            targets_reached_by_tests.insert(def.path.clone(), reachable_targets);
        }

        Self {
            unique_source_file_paths,
            local_source_file_paths,
            mutations_per_file,
            mutations_on_overlapping_lines,
            definitions,
            tests,
            call_graph: call_graph_metadata.call_graph.clone(),
            targets,
            targets_reached_by_tests,
            mutations_per_target,
            mutations: IdxVec::new(),
            evaluation_info: None,
        }
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
    pub fn call_graph(&self) -> &CallGraph {
        &self.call_graph
    }

    #[inline]
    pub fn targets_reached_by_test(&self, def_path: &str) -> &[TargetId] {
        self.targets_reached_by_tests.get(def_path).map(|vec| -> &[_] { vec }).unwrap_or_default()
    }

    #[inline]
    pub fn target(&self, target_id: TargetId) -> Option<&Target> {
        self.targets.get(target_id)
    }

    #[inline]
    pub fn target_mutations(&self, target_id: TargetId) -> &[MutationId] {
        self.mutations_per_target.get(target_id).map(|vec| -> &[_] { vec }).unwrap_or_default()
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
