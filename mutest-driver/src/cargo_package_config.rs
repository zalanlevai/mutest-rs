use std::env;

use mutest_emit::codegen::mutation::OperatorRef;
use rustc_session::EarlyDiagCtxt;
use serde::{Deserialize, Deserializer};

#[derive(Debug)]
enum MutationOperatorWithOptions<T> {
    EnabledWithDefaults(bool),
    EnabledWithOptions(T),
}

impl<'de, T> Deserialize<'de> for MutationOperatorWithOptions<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        serde_untagged::UntaggedEnumVisitor::new()
            .expecting("a boolean value or a record of options")
            .bool(|v| Ok(MutationOperatorWithOptions::EnabledWithDefaults(v)))
            .map(|v| v.deserialize().map(MutationOperatorWithOptions::EnabledWithOptions))
            .deserialize(deserializer)
    }
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct CallIgnoreOptions {
    limit_scope_to_local_callees: bool,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct MutationOperators {
    arg_default_shadow: Option<bool>,
    bit_op_or_and_swap: Option<bool>,
    bit_op_or_xor_swap: Option<bool>,
    bit_op_shift_dir_swap: Option<bool>,
    bit_op_xor_and_swap: Option<bool>,
    bool_expr_negate: Option<bool>,
    call_delete: Option<MutationOperatorWithOptions<CallIgnoreOptions>>,
    call_value_default_shadow: Option<MutationOperatorWithOptions<CallIgnoreOptions>>,
    continue_break_swap: Option<bool>,
    eq_op_invert: Option<bool>,
    logical_op_and_or_swap: Option<bool>,
    math_op_add_mul_swap: Option<bool>,
    math_op_add_sub_swap: Option<bool>,
    math_op_div_rem_swap: Option<bool>,
    math_op_mul_div_swap: Option<bool>,
    range_limit_swap: Option<bool>,
    relational_op_eq_swap: Option<bool>,
    relational_op_invert: Option<bool>,
}

impl MutationOperators {
    fn into_operators<'op, 'm>(&self) -> Vec<OperatorRef<'op, 'm>> {
        // NOTE: Mutation operators must be sorted into a deterministic order,
        //       because that determines application order, and thus mutation order.
        //       We use an alphabetical order for this based on operator names,
        //       so pushes must be sorted accordingly.
        let mut ops = Vec::<OperatorRef<'op, 'm>>::new();

        if let Some(true) = &self.arg_default_shadow {
            ops.push(Box::leak(Box::new(mutest_operators::ArgDefaultShadow)));
        }
        if let Some(true) = &self.bit_op_or_and_swap {
            ops.push(Box::leak(Box::new(mutest_operators::BitOpOrAndSwap)))
        }
        if let Some(true) = &self.bit_op_or_xor_swap {
            ops.push(Box::leak(Box::new(mutest_operators::BitOpOrXorSwap)))
        }
        if let Some(true) = &self.bit_op_shift_dir_swap {
            ops.push(Box::leak(Box::new(mutest_operators::BitOpShiftDirSwap)))
        }
        if let Some(true) = &self.bit_op_xor_and_swap {
            ops.push(Box::leak(Box::new(mutest_operators::BitOpXorAndSwap)))
        }
        if let Some(true) = &self.bool_expr_negate {
            ops.push(Box::leak(Box::new(mutest_operators::BoolExprNegate)))
        }
        if let Some(call_ignore_opts) = &self.call_delete {
            'v: {
                let call_ignore_opts = match call_ignore_opts {
                    MutationOperatorWithOptions::EnabledWithDefaults(false) => break 'v,
                    MutationOperatorWithOptions::EnabledWithDefaults(true) => &Default::default(),
                    MutationOperatorWithOptions::EnabledWithOptions(opts) => opts,
                };

                ops.push(Box::leak(Box::new(mutest_operators::CallDelete {
                    limit_scope_to_local_callees: call_ignore_opts.limit_scope_to_local_callees,
                })));
            }
        }
        if let Some(call_ignore_opts) = &self.call_value_default_shadow {
            'v: {
                let call_ignore_opts = match call_ignore_opts {
                    MutationOperatorWithOptions::EnabledWithDefaults(false) => break 'v,
                    MutationOperatorWithOptions::EnabledWithDefaults(true) => &Default::default(),
                    MutationOperatorWithOptions::EnabledWithOptions(opts) => opts,
                };

                ops.push(Box::leak(Box::new(mutest_operators::CallValueDefaultShadow {
                    limit_scope_to_local_callees: call_ignore_opts.limit_scope_to_local_callees,
                })));
            }
        }
        if let Some(true) = &self.continue_break_swap {
            ops.push(Box::leak(Box::new(mutest_operators::ContinueBreakSwap)))
        }
        if let Some(true) = &self.eq_op_invert {
            ops.push(Box::leak(Box::new(mutest_operators::EqOpInvert)))
        }
        if let Some(true) = &self.logical_op_and_or_swap {
            ops.push(Box::leak(Box::new(mutest_operators::LogicalOpAndOrSwap)))
        }
        if let Some(true) = &self.math_op_add_mul_swap {
            ops.push(Box::leak(Box::new(mutest_operators::OpAddMulSwap)))
        }
        if let Some(true) = &self.math_op_add_sub_swap {
            ops.push(Box::leak(Box::new(mutest_operators::OpAddSubSwap)))
        }
        if let Some(true) = &self.math_op_div_rem_swap {
            ops.push(Box::leak(Box::new(mutest_operators::OpDivRemSwap)))
        }
        if let Some(true) = &self.math_op_mul_div_swap {
            ops.push(Box::leak(Box::new(mutest_operators::OpMulDivSwap)))
        }
        if let Some(true) = &self.range_limit_swap {
            ops.push(Box::leak(Box::new(mutest_operators::RangeLimitSwap)))
        }
        if let Some(true) = &self.relational_op_eq_swap {
            ops.push(Box::leak(Box::new(mutest_operators::RelationalOpEqSwap)))
        }
        if let Some(true) = &self.relational_op_invert {
            ops.push(Box::leak(Box::new(mutest_operators::RelationalOpInvert)))
        }

        ops
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum GreedyMutationBatchingOrderingHeuristic {
    None,
    Conflicts,
    ReverseConflicts,
    Random,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case", rename_all_fields = "kebab-case")]
#[serde(tag = "batching-algorithm")]
pub enum MutationBatchingAlgorithm {
    Random,
    Greedy {
        greedy_batching_ordering_heuristic: Option<GreedyMutationBatchingOrderingHeuristic>,
        greedy_batching_epsilon: Option<f64>,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case", rename_all_fields = "kebab-case")]
#[serde(tag = "mode")]
pub enum MutationParallelism {
    None,
    Batching {
        batch_size: Option<usize>,
        #[serde(flatten)]
        batching_algorithm: Option<MutationBatchingAlgorithm>,
        batching_seed: Option<String>,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct CargoMutestMetadata {
    call_graph_depth_limit: Option<usize>,
    call_graph_trace_length_limit: Option<usize>,
    mutation_depth: Option<usize>,
    mutation_operators: Option<MutationOperators>,
    mutation_parallelism: Option<MutationParallelism>,
}

pub struct MergedCargoPackageConfig<'op, 'm> {
    pub call_graph_depth_limit: Option<usize>,
    pub call_graph_trace_length_limit: Option<usize>,
    pub mutation_depth: Option<usize>,
    pub mutation_operators: Option<Vec<OperatorRef<'op, 'm>>>,
    pub mutation_parallelism: Option<MutationParallelism>,
}

pub fn fetch_merged_cargo_package_config<'op, 'm>(early_dcx: &EarlyDiagCtxt) -> Option<MergedCargoPackageConfig<'op, 'm>> {
    let cargo_package_manifest_path_str = env::var("CARGO_MANIFEST_PATH").ok()?;

    let mut metadata_cmd = cargo_metadata::MetadataCommand::new();
    metadata_cmd.manifest_path(&cargo_package_manifest_path_str);
    metadata_cmd.no_deps();

    let metadata = metadata_cmd.exec().expect("could not retrieve Cargo metadata");

    let Some(package) = metadata.packages.iter().find(|package| package.manifest_path == cargo_package_manifest_path_str) else {
        panic!("cannot find package in Cargo metadata");
    };
    let mut package_mutest_metadata = match &package.metadata {
        serde_json::Value::Object(object) if let Some(mutest_metadata) = object.get("mutest") => {
            match serde_path_to_error::deserialize::<_, CargoMutestMetadata>(mutest_metadata) {
                Ok(mutest_metadata) => Some(mutest_metadata),
                Err(error) => {
                    let package_manifest_path = match package.manifest_path.strip_prefix(&metadata.workspace_root) {
                        Ok(relative_path) => relative_path,
                        Err(_) => &package.manifest_path,
                    };

                    let mut diagnostic = early_dcx.early_struct_fatal(format!("{error}"));
                    diagnostic.note(format!("while parsing `package.metadata.mutest` in Cargo package manifest `{}`", package_manifest_path));
                    diagnostic.emit();
                }
            }
        }
        _ => None,
    };

    let mut workspace_mutest_metadata = match &metadata.workspace_metadata {
        serde_json::Value::Object(object) if let Some(mutest_metadata) = object.get("mutest") => {
            match serde_path_to_error::deserialize::<_, CargoMutestMetadata>(mutest_metadata) {
                Ok(mutest_metadata) => Some(mutest_metadata),
                Err(error) => {
                    let mut diagnostic = early_dcx.early_struct_fatal(format!("{error}"));
                    diagnostic.note("while parsing `workspace.metadata.mutest` in Cargo workspace manifest");
                    diagnostic.emit();
                }
            }
        }
        _ => None,
    };

    if package_mutest_metadata.is_none() && workspace_mutest_metadata.is_none() { return None; }

    let merged_cargo_package_config = MergedCargoPackageConfig {
        call_graph_depth_limit: package_mutest_metadata.as_ref().and_then(|m| m.call_graph_depth_limit)
            .or_else(|| workspace_mutest_metadata.as_ref().and_then(|m| m.call_graph_depth_limit)),
        call_graph_trace_length_limit: package_mutest_metadata.as_ref().and_then(|m| m.call_graph_trace_length_limit)
            .or_else(|| workspace_mutest_metadata.as_ref().and_then(|m| m.call_graph_trace_length_limit)),
        mutation_depth: package_mutest_metadata.as_ref().and_then(|m| m.mutation_depth)
            .or_else(|| workspace_mutest_metadata.as_ref().and_then(|m| m.mutation_depth)),
        mutation_operators: package_mutest_metadata.as_ref().and_then(|m| m.mutation_operators.as_ref())
            .or_else(|| workspace_mutest_metadata.as_ref().and_then(|m| m.mutation_operators.as_ref()))
            .map(|mutation_operators| mutation_operators.into_operators()),
        mutation_parallelism: package_mutest_metadata.as_mut().and_then(|m| m.mutation_parallelism.take())
            .or_else(|| workspace_mutest_metadata.as_mut().and_then(|m| m.mutation_parallelism.take())),
    };

    if let Some(mutation_operators) = &merged_cargo_package_config.mutation_operators && mutation_operators.is_empty() {
        let mut diagnostic = early_dcx.early_struct_fatal("all mutation operators are disabled in Cargo package manifest");
        diagnostic.note("consider enabling a mutation operator, or removing the empty table in the Cargo package manifest");
        diagnostic.emit();
    }

    Some(merged_cargo_package_config)
}
