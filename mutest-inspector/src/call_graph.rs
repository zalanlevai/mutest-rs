use std::collections::HashSet;
use std::fmt::Write;

use mutest_json::DefId;
use mutest_json::call_graph::{CallGraph, EntryPoint, EntryPointId, CalleeId};
use mutest_json::mutations::MutationId;

#[derive(Clone, Debug)]
pub struct MonoCallTrace {
    pub entry_point_id: EntryPointId,
    pub nested_calls: Vec<CalleeId>,
}

pub(crate) fn build_mono_call_traces(call_graph: &CallGraph, mono_call_traces: &mut Vec<MonoCallTrace>, mono_call_trace: &mut MonoCallTrace, target_def_id: DefId) {
    let [.., callee_id] = &mono_call_trace.nested_calls[..] else { return; };
    let callee = &call_graph.callees[*callee_id];

    if callee.def_id == target_def_id {
        mono_call_traces.push(mono_call_trace.clone());
        return;
    }

    for (nested_callee_id, _) in &callee.calls {
        if mono_call_trace.nested_calls.iter().any(|callee_id| callee_id == nested_callee_id) { continue; }

        mono_call_trace.nested_calls.push(*nested_callee_id);
        build_mono_call_traces(call_graph, mono_call_traces, mono_call_trace, target_def_id);
        mono_call_trace.nested_calls.pop();
    }
}

pub fn entry_point_mono_call_traces(call_graph: &CallGraph, entry_point: &EntryPoint, target_def_id: DefId) -> Vec<MonoCallTrace> {
    let mut mono_call_traces = Vec::new();

    for (callee_id, _) in &entry_point.calls {
        let mut mono_call_trace = MonoCallTrace { entry_point_id: entry_point.entry_point_id, nested_calls: vec![*callee_id] };
        build_mono_call_traces(&call_graph, &mut mono_call_traces, &mut mono_call_trace, target_def_id);
    }

    mono_call_traces
}

#[derive(Clone, Debug)]
#[derive(Eq, PartialEq, Hash)]
pub struct DefCallTrace {
    pub entry_point_id: EntryPointId,
    pub nested_calls: Vec<DefId>,
}

pub fn reduce_mono_call_traces(call_graph: &CallGraph, mono_call_traces: &[MonoCallTrace]) -> Vec<DefCallTrace> {
    let mut def_call_traces = HashSet::new();

    for mono_call_trace in mono_call_traces {
        let nested_calls = mono_call_trace.nested_calls.iter()
            .map(|callee_id| {
                let callee = &call_graph.callees[*callee_id];
                callee.def_id
            })
            .collect();
        def_call_traces.insert(DefCallTrace { entry_point_id: mono_call_trace.entry_point_id, nested_calls });
    }

    def_call_traces.into_iter().collect()
}

pub struct TraceSpec<'a> {
    pub entry_point_def_path: &'a str,
    pub callee_def_ids: Vec<DefId>,
    pub mutation_id: MutationId,
}

impl<'a> TraceSpec<'a> {
    pub fn write(&self, out: &mut String) {
        write!(out, "{}", self.entry_point_def_path).unwrap();
        for &def_id in &self.callee_def_ids {
            write!(out, "->D{}", def_id.0).unwrap();
        }
        write!(out, "+M{}", self.mutation_id.0).unwrap();
    }

    pub fn to_string(&self) -> String {
        let mut s = String::new();
        self.write(&mut s);
        s
    }

    pub fn parse(trace_spec_str: &'a str) -> Option<Self> {
        let (entry_point_def_path, rest) = trace_spec_str.split_once("->")?;
        let (trace_str, mutation_id_str) = rest.split_once('+')?;
        let callee_def_ids = trace_str.split("->").filter_map(|entry_str| entry_str.strip_prefix('D')?.parse::<u32>().ok()).map(mutest_json::DefId).collect::<Vec<_>>();
        let mutation_id = MutationId(mutation_id_str.strip_prefix('M')?.parse::<u32>().ok()?);

        if callee_def_ids.is_empty() { return None; }

        Some(Self { entry_point_def_path, callee_def_ids, mutation_id })
    }

    pub fn root_callee(&self) -> DefId {
        *self.callee_def_ids.first().expect("invalid trace spec: no callees")
    }

    pub fn target(&self) -> DefId {
        *self.callee_def_ids.last().expect("invalid trace spec: no callees")
    }
}
