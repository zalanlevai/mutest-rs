use mutest_emit::analysis::hir;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::harness::mk_crate_kind_const;
use mutest_emit::codegen::symbols::{DUMMY_SP, Ident, Symbol, sym};
use rustc_middle::ty::TyCtxt;
use thin_vec::thin_vec;

use crate::passes::external_mutant::RustcInvocation;

pub fn embed_rustc_invocation(krate: &mut ast::Crate, rustc_invocation: &RustcInvocation) {
    // pub const CRATE_KIND: &str = "recompilable_dep_crate";
    let crate_kind_const = mk_crate_kind_const(DUMMY_SP, "recompilable_dep_crate");

    // pub const RUSTC_ARGS: &str = "arg1\x1F...";
    let rustc_args_const = {
        let rustc_args_str = rustc_invocation.args.iter().map(|s| -> &str { s }).intersperse("\x1F").collect::<String>();

        let vis = ast::mk::vis_pub(DUMMY_SP);
        let ident = Ident::new(Symbol::intern("RUSTC_ARGS"), DUMMY_SP);
        let ty = ast::mk::ty_ref(DUMMY_SP, ast::mk::ty_ident(DUMMY_SP, None, Ident::new(sym::str, DUMMY_SP)), None);
        let expr = ast::mk::expr_str(DUMMY_SP, &rustc_args_str);
        ast::mk::item_const(DUMMY_SP, vis, ident, ty, expr)
    };

    // pub const RUSTC_ENV_VARS: &str = "KEY\x1Fval\x1F...";
    let rustc_env_vars_const = {
        let rustc_env_vars_str = rustc_invocation.env_vars.iter().flat_map(|(k, v)| [k, v]).map(|s| -> &str { s }).intersperse("\x1F").collect::<String>();

        let vis = ast::mk::vis_pub(DUMMY_SP);
        let ident = Ident::new(Symbol::intern("RUSTC_ENV_VARS"), DUMMY_SP);
        let ty = ast::mk::ty_ref(DUMMY_SP, ast::mk::ty_ident(DUMMY_SP, None, Ident::new(sym::str, DUMMY_SP)), None);
        let expr = ast::mk::expr_str(DUMMY_SP, &rustc_env_vars_str);
        ast::mk::item_const(DUMMY_SP, vis, ident, ty, expr)
    };

    // pub mod mutest_generated { ... }
    let mutest_generated_mod = ast::mk::item_mod(DUMMY_SP,
        ast::mk::vis_pub(DUMMY_SP),
        Ident::new(sym::mutest_generated, DUMMY_SP),
        thin_vec![
            crate_kind_const,
            rustc_args_const,
            rustc_env_vars_const,
        ],
    );

    krate.items.push(mutest_generated_mod);
}

pub fn extract_rustc_invocation<'tcx>(tcx: TyCtxt<'tcx>, cnum: hir::CrateNum) -> Option<RustcInvocation> {
    let mutest_generated_mod = tcx.module_children(cnum.as_def_id()).iter().find_map(|mod_child| {
        if mod_child.ident.name != sym::mutest_generated { return None; }
        mod_child.res.mod_def_id()
    })?;

    let crate_kind_const = tcx.module_children(mutest_generated_mod).iter().find_map(|mod_child| {
        if mod_child.ident.name != sym::CRATE_KIND { return None; }
        let hir::Res::Def(hir::DefKind::Const, def_id) = mod_child.res else { return None; };
        Some(def_id)
    })?;
    let Ok(crate_kind_val) = tcx.const_eval_poly(crate_kind_const) else { unreachable!() };
    let Some(crate_kind_bytes) = crate_kind_val.try_get_slice_bytes_for_diagnostics(tcx) else { unreachable!() };
    let Ok(crate_kind) = str::from_utf8(crate_kind_bytes) else { tcx.dcx().fatal("invalid UTF-8 in rustc invocation metadata") };
    if crate_kind != "recompilable_dep_crate" { return None; }

    let Some(rustc_args_const) = tcx.module_children(mutest_generated_mod).iter().find_map(|mod_child| {
        if mod_child.ident.name != Symbol::intern("RUSTC_ARGS") { return None; }
        let hir::Res::Def(hir::DefKind::Const, def_id) = mod_child.res else { return None; };
        Some(def_id)
    }) else {
        let mut diagnostic = tcx.dcx().struct_fatal("missing `mutest_generated::RUSTC_ARGS` in recompilable dependency crate");
        diagnostic.note("target mutant crate must be compiled with mutest-driver with --crate-kind=mutable-dep-for-external-tests to ensure the necessary embeddings are in place");
        diagnostic.emit();
    };
    let Ok(rustc_args_val) = tcx.const_eval_poly(rustc_args_const) else { unreachable!() };
    let Some(rustc_args_bytes) = rustc_args_val.try_get_slice_bytes_for_diagnostics(tcx) else { unreachable!() };

    let Ok(rustc_args) = str::from_utf8(rustc_args_bytes) else { tcx.dcx().fatal("invalid UTF-8 in rustc invocation metadata") };
    let rustc_args = rustc_args.split("\x1F").map(|s| s.to_owned()).collect::<Vec<_>>();

    let Some(rustc_env_vars_const) = tcx.module_children(mutest_generated_mod).iter().find_map(|mod_child| {
        if mod_child.ident.name != Symbol::intern("RUSTC_ENV_VARS") { return None; }
        let hir::Res::Def(hir::DefKind::Const, def_id) = mod_child.res else { return None; };
        Some(def_id)
    }) else {
        let mut diagnostic = tcx.dcx().struct_fatal("missing `mutest_generated::RUSTC_ENV_VARS` in recompilable dependency crate");
        diagnostic.note("target mutant crate must be compiled with mutest-driver with --crate-kind=mutable-dep-for-external-tests to ensure the necessary embeddings are in place");
        diagnostic.emit();
    };
    let Ok(rustc_env_vars_val) = tcx.const_eval_poly(rustc_env_vars_const) else { unreachable!() };
    let Some(rustc_env_vars_bytes) = rustc_env_vars_val.try_get_slice_bytes_for_diagnostics(tcx) else { unreachable!() };

    let Ok(rustc_env_vars) = str::from_utf8(rustc_env_vars_bytes) else { tcx.dcx().fatal("invalid UTF-8 in rustc invocation metadata") };
    let rustc_env_vars = rustc_env_vars.split("\x1F").array_chunks::<2>().map(|[k, v]| (k.to_owned(), v.to_owned())).collect::<Vec<_>>();

    Some(RustcInvocation {
        args: rustc_args,
        env_vars: rustc_env_vars,
    })
}
