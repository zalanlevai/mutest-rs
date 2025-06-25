use std::iter;
use std::mem;
use std::ops::DerefMut;
use std::panic;
use std::sync::Arc;

use rustc_data_structures::flat_map_in_place::FlatMapInPlace;
use rustc_expand::base::{SyntaxExtension, SyntaxExtensionKind};
use rustc_hash::FxHashSet;
use rustc_infer::infer::TyCtxtInferExt;
use rustc_metadata::creader::{CStore, LoadedMacro};
use rustc_middle::span_bug;
use rustc_middle::ty::TypingMode;
use rustc_span::edition::Edition;
use rustc_trait_selection::traits::{ImplSource, Obligation, ObligationCause, SelectionContext};
use smallvec::{SmallVec, smallvec};
use thin_vec::{ThinVec, thin_vec};

use crate::analysis::ast_lowering;
use crate::analysis::hir::{self, LOCAL_CRATE, NodeExt};
use crate::analysis::res;
use crate::analysis::ty::{self, Ty, TyCtxt};
use crate::codegen::ast::{self, P};
use crate::codegen::ast::mut_visit::MutVisitor;
use crate::codegen::symbols::{DUMMY_SP, ExpnKind, Ident, Span, Symbol, sym, kw};
use crate::codegen::symbols::hygiene::{ExpnData, ExpnId, MacroKind, Transparency};

fn is_macro_expn(expn: &ExpnData) -> bool {
    match expn.kind {
        ExpnKind::Macro(_, _) => true,

        | ExpnKind::Root
        | ExpnKind::AstPass(_)
        | ExpnKind::Desugaring(_)
        => false,
    }
}

enum IdentResKind {
    Local,
    Label,
    Def,
}

fn sanitize_ident_if_from_expansion(ident: &mut Ident, ident_res_kind: IdentResKind) {
    // `crate`, `self`, `super` and `Self` are keywords that cannot be raw identifiers, see
    // https://doc.rust-lang.org/reference/identifiers.html.
    if ident.name == kw::Crate { return; }
    if ident.name == kw::Super { return; }
    if ident.name == kw::SelfLower || ident.name == kw::SelfUpper { return; }
    // `_` is an ident but cannot be a raw identifier and cannot be used in paths.
    if ident.name == kw::Underscore || ident.name == kw::UnderscoreLifetime { return; }

    let syntax_ctxt = ident.span.ctxt();

    let mut res_ctxt_expn_id = None;
    for (expn_id, transparency) in syntax_ctxt.marks().into_iter().rev() {
        // Some identifiers produced by semi-transparent expansions (e.g. macro_rules macros)
        // may be resolved at call-site, rather than at definition-site, meaning that
        // they are not actually hygienic.
        if let Transparency::SemiOpaque = transparency {
            // Local variables and labels get resolved at definition-site (i.e. hygienic),
            // everything else at call-site (i.e. not hygienic).
            // NOTE: `$crate` also gets resolved at definition-site but is handled
            //       before our sanitization step.
            let (IdentResKind::Local | IdentResKind::Label) = ident_res_kind else { continue; };
        }

        res_ctxt_expn_id = Some(expn_id);
        break;
    }

    let Some(res_ctxt_expn_id) = res_ctxt_expn_id else { return; };
    let res_ctxt_expn = res_ctxt_expn_id.expn_data();
    if !is_macro_expn(&res_ctxt_expn) { return; }

    let (is_label, bare_ident) = match ident.as_str().strip_prefix("'") {
        Some(bare_ident) => (true, bare_ident),
        None => (false, ident.as_str()),
    };

    if bare_ident.starts_with("__rustc_expn_") {
        span_bug!(ident.span, "encountered ident starting with `__rustc_expn`: the ident might have been sanitized twice");
    }

    ident.name = Symbol::intern(&format!("{prefix}__rustc_expn_{expn_crate_id}_{expn_local_id}_{bare_ident}",
        prefix = is_label.then(|| "'").unwrap_or(""),
        expn_crate_id = res_ctxt_expn_id.krate.index(),
        expn_local_id = res_ctxt_expn_id.local_id.index(),
    ));
}

/// Copy syntax context of definition-site ident span to use-site ident
/// so that the use-site ident can be appropriately sanitized by
/// `sanitize_ident_if_from_expansion`.
fn copy_def_span_ctxt(ident: &mut Ident, def_ident_span: Span) {
    ident.span = ident.span.with_ctxt(def_ident_span.ctxt());
}

pub fn sanitize_ident_if_def_from_expansion(ident: &mut Ident, def_ident_span: Span) {
    copy_def_span_ctxt(ident, def_ident_span);
    sanitize_ident_if_from_expansion(ident, IdentResKind::Def);
}

fn sanitize_standalone_ident_if_from_expansion(ident: &mut Ident, ident_res_kind: IdentResKind) {
    if ident.name == kw::StaticLifetime { return; }

    sanitize_ident_if_from_expansion(ident, ident_res_kind);
}

fn is_macro_helper_attr(syntax_extensions: &[SyntaxExtension], attr: &ast::Attribute) -> bool {
    syntax_extensions.iter().any(|syntax_extension| {
        syntax_extension.helper_attrs.iter().any(|&helper_attr| attr.has_name(helper_attr))
    })
}

#[derive(Clone, Copy)]
enum DefPathRequestKind {
    Item(hir::DefId),
    ParentModStub(hir::ModDefId),
}

#[derive(Clone, Copy)]
enum Macros2_0TopLevelRelativePathResHack {
    InTopLevelMacros2_0Scope { parent_module: hir::DefId },
    InNestedMacros2_0Scope(ExpnId),
    NotInMacros2_0Scope,
}

struct MacroExpansionSanitizer<'tcx, 'op> {
    tcx: TyCtxt<'tcx>,
    crate_res: &'op res::CrateResolutions<'tcx>,
    def_res: &'op ast_lowering::DefResolutions,
    /// Body resolutions for interfacing with the HIR.
    body_res: &'op ast_lowering::BodyResolutions<'tcx>,
    syntax_extensions: Vec<SyntaxExtension>,
    /// The prelude module of this crate (marked with `#[prelude_import]`)
    /// whose contents are available in every module, used for import path resolution.
    prelude_mod: Option<hir::DefId>,

    /// Keep track of the current scope (e.g. items and bodies) for relative name resolution.
    current_scope: Option<hir::DefId>,
    /// Keep track of the current type checking context for type-dependent name resolution.
    current_typeck_ctx: Option<&'tcx ty::TypeckResults<'tcx>>,

    /// In macros 2.0 scopes, relative paths get resolved in the scope of the macro definining module,
    /// but only at the top-level within the macro expansion.
    /// HACK: We use a marker to determine whether we are in a macros 2.0 scope, but not in a module nested inside.
    /// NOTE: This is because rustc currently does not accept (and correctly resolve) relative paths
    ///       in nested modules within macros 2.0 macro expansion scopes.
    ///       See `tests/ui/hygiene/rustc_res/relative_use_imports_do_not_work_in_nested_mod_within_macros_2_0`
    ///       to test whether this expectation is still met.
    macros_2_0_top_level_relative_path_res_hack: Macros2_0TopLevelRelativePathResHack,

    /// We do not want to sanitize some idents (mostly temporarily) in the AST.
    /// During the visit we keep track of these so that they can be exluded from sanitization.
    protected_idents: FxHashSet<Ident>,
}

impl<'tcx, 'op> MacroExpansionSanitizer<'tcx, 'op> {
    #[must_use]
    fn overwrite_path_with_def_path(&self, path: &mut ast::Path, def_path: &res::DefPath<'tcx>) -> Option<Ty<'tcx>> {
        let mut segments_with_generics = match &path.segments[..] {
            // NOTE: We emit dummy, empty "source" paths when constructing import paths.
            // FIXME: Turn into ICE once this mechanism for import paths is not used anymore.
            [] => vec![],

            [path_segments @ .., last_path_segment] => {
                let mut segments_with_generics = path_segments.into_iter()
                    .filter_map(|segment| {
                        let args = segment.args.as_ref()?;
                        let res = self.def_res.node_res(segment.id)?;
                        let def_id = res.opt_def_id()?;
                        Some((def_id, Some(args.clone())))
                    })
                    .collect::<Vec<_>>();

                // Ensure that we always retain the final, "item" generic arguments, even if
                // the last path segment itself does not have a sufficient resolution, since
                // we can use the final path resolution instead.
                if let [.., last_def_path_segment] = &def_path.segments[..] {
                    let mut def_id = last_def_path_segment.def_id;
                    if let hir::DefKind::Ctor(..) = self.tcx.def_kind(def_id) {
                        // Adjust target definition to the parent, mirroring what is done in `adjust_path_from_expansion`,
                        // so that the segment def ids match up.
                        def_id = self.tcx.parent(def_id);
                    }
                    segments_with_generics.push((def_id, last_path_segment.args.clone()));
                }

                // Ensure that we always retain the generic arguments provided to an enum, even if
                // the enum is not present in the def path and the enum variant is referred to directly
                // (e.g. if there is a visible re-export of the enum variant).
                // NOTE: The enum generic args can always be provided to the enum variant instead, so
                //       we always just use this form instead, see
                //       `tests/ui/hygiene/rustc_res/enum_variant_may_be_available_without_enum_being_visible`.
                if let [.., (enum_def_id, enum_args), (enum_variant_def_id, enum_variant_args)] = &mut segments_with_generics[..]
                    && let hir::DefKind::Enum = self.tcx.def_kind(*enum_def_id)
                    && let hir::DefKind::Variant = self.tcx.def_kind(*enum_variant_def_id)
                    && let None = enum_variant_args
                {
                    *enum_variant_args = enum_args.take();
                }

                segments_with_generics
            }
        };

        let mut segments = def_path.segments.iter()
            .flat_map(|def_path_segment| {
                let def_id = def_path_segment.def_id;

                let mut ident = match def_path_segment.reexport {
                    None => {
                        // NOTE: The only reason we do not use `tcx.opt_item_ident(def_id)?` here is that
                        //       it panics if no span is found, which happens for crate root defs.
                        let span = self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                        let name = match () {
                            // Keep special `Self` keyword segments in path from def path resolution.
                            _ if def_path_segment.ident.name == kw::SelfUpper => kw::SelfUpper,

                            _ => self.tcx.opt_item_name(def_id)?,
                        };
                        // TODO: Use path.span with copy_def_span_ctxt.
                        Ident::new(name, span)
                    }
                    Some(_) => def_path_segment.ident,
                };
                sanitize_ident_if_from_expansion(&mut ident, IdentResKind::Def);

                let mut segment = ast::PathSegment { id: ast::DUMMY_NODE_ID, ident, args: None };

                match segments_with_generics.extract_if(.., |(segment_def_id, _)| *segment_def_id == def_id).next() {
                    // Copy matching generic args from the corresponding segment in the original path.
                    Some((_, Some(mut args))) => {
                        // Sanitize associated constraint idents.
                        match args.deref_mut() {
                            ast::GenericArgs::AngleBracketed(args) => 'arm: {
                                // Skip sanitization if it is only an argument list and there are no references to assoc items.
                                // NOTE: This is also needed to avoid attempting to fetch assoc items for e.g. generic function calls.
                                if !args.args.iter().any(|arg| matches!(arg, ast::AngleBracketedArg::Constraint(..))) { break 'arm; }

                                let assoc_items = self.tcx.associated_items(def_id);

                                for arg in &mut args.args {
                                    match arg {
                                        ast::AngleBracketedArg::Constraint(assoc_constraint) => {
                                            let assoc_tag = match &assoc_constraint.kind {
                                                ast::AssocItemConstraintKind::Equality { term } => {
                                                    match term {
                                                        ast::Term::Ty(..) => ty::AssocTag::Type,
                                                        ast::Term::Const(..) => ty::AssocTag::Const,
                                                    }
                                                }
                                                ast::AssocItemConstraintKind::Bound { .. } => ty::AssocTag::Type,
                                            };

                                            if let Some(assoc_item) = assoc_items
                                                .filter_by_name_unhygienic(assoc_constraint.ident.name)
                                                .find(|assoc_item| assoc_item.as_tag() == assoc_tag)
                                            {
                                                // Copy and sanitize assoc item definition ident.
                                                let Some(assoc_item_ident_span) = self.tcx.def_ident_span(assoc_item.def_id) else { unreachable!() };
                                                copy_def_span_ctxt(&mut assoc_constraint.ident, assoc_item_ident_span);
                                                sanitize_ident_if_from_expansion(&mut assoc_constraint.ident, IdentResKind::Def);
                                            }
                                        }
                                        ast::AngleBracketedArg::Arg(_) => {}
                                    }
                                }
                            }
                            ast::GenericArgs::Parenthesized(_) | ast::GenericArgs::ParenthesizedElided(_) => {}
                        }

                        // Copy modified args.
                        segment.args = Some(args);
                    }

                    // Corresponding segment in the original path has no generic args, leave as-is.
                    Some((_, None)) => {}

                    // No corresponding segment in the original path, which can only occur for intermediate path components.
                    // NOTE: We used to add dummy generics with type inference holes for the non-synthetic generics of the def, but
                    //       this is not needed for intermediate path components, and
                    //       the final path component, which needed this, is now guaranteed to have its relevant args.
                    None => {}
                }

                Some(segment)
            })
            .collect::<ThinVec<_>>();

        let mut qself = None;
        match &def_path.root {
            // `crate::..` paths.
            &res::DefPathRootKind::Global(cnum) if cnum == LOCAL_CRATE => {
                segments.insert(0, ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: Ident::new(kw::Crate, DUMMY_SP), args: None });
            }
            // `::<crate>::..` paths.
            &res::DefPathRootKind::Global(cnum) => {
                let crate_name = self.crate_res.visible_crate_name(cnum);
                segments.splice(0..0, [
                    ast::PathSegment::path_root(DUMMY_SP),
                    ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: Ident::new(crate_name, DUMMY_SP), args: None },
                ]);
            }

            // `<ty>::..` paths to inherent impl assoc items.
            &res::DefPathRootKind::Ty(ty) => {
                // NOTE: We return the type root of the path to the caller as an indicator that, if present,
                //       something must be done to create a complete path.
                qself = Some(ty);
            }

            // Local path, no special prefix.
            res::DefPathRootKind::Local => {}

            // `super::..` paths.
            res::DefPathRootKind::Parent { supers } => {
                segments.splice(0..0, (0..*supers).map(|_| ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: Ident::new(kw::Super, DUMMY_SP), args: None }));
            }
        }

        *path = ast::Path { span: path.span, segments, tokens: None };

        if path.segments.is_empty() {
            span_bug!(path.span, "path was sanitized into an empty path");
        }

        qself
    }

    fn expect_visible_def_path(&self, request: DefPathRequestKind, span: Span, ignore_reexport: Option<hir::DefId>) -> res::DefPath<'tcx> {
        let def_id = match request {
            DefPathRequestKind::Item(def_id) => def_id,
            DefPathRequestKind::ParentModStub(def_id) => def_id.to_def_id(),
        };

        // Prefer using a direct, local path to local items within the same module as the enclosing module (or parent modules) of the current scope.
        // NOTE: This helps avoid visibility-related resolution issues in local items, see
        //       `tests/ui/hygiene/rustc_res/private_ctor_not_available_in_same_scope_through_reexport`, and
        //       `tests/ui/hygiene/rustc_res/private_ctor_not_available_in_child_scope_through_reexport`.
        if let Some(current_scope) = self.current_scope && let Some(local_def_id) = def_id.as_local() && def_id != current_scope {
            let mod_scope = match self.tcx.def_kind(current_scope) {
                hir::DefKind::Mod => current_scope,
                _ => self.tcx.parent_module_from_def_id(current_scope.expect_local()).to_def_id(),
            };
            let containing_mod = match request {
                DefPathRequestKind::Item(_) => self.tcx.parent_module_from_def_id(local_def_id).to_def_id(),
                DefPathRequestKind::ParentModStub(_) => def_id,
            };

            if containing_mod == mod_scope {
                if let Ok(visible_path) = res::locally_visible_def_path(self.tcx, def_id, current_scope) {
                    return visible_path;
                }
            } else if !containing_mod.is_crate_root() {
                let is_locally_accessible_through_supers = 'v: {
                    let mut parent_mod_scope = mod_scope;
                    let mut super_mods = vec![];

                    while parent_mod_scope != containing_mod {
                        let parent_mod = self.tcx.parent_module_from_def_id(parent_mod_scope.expect_local()).to_def_id();
                        if parent_mod.is_crate_root() {
                            break 'v None;
                        }

                        super_mods.push(parent_mod);
                        parent_mod_scope = parent_mod;
                    }

                    Some(super_mods)
                };

                if let Some(super_mods) = is_locally_accessible_through_supers {
                    match request {
                        DefPathRequestKind::Item(_) => {
                            if let Ok(mut visible_path) = res::locally_visible_def_path(self.tcx, def_id, containing_mod) {
                                // Construct path to containing parent module, which are
                                // always accessible through consecutive `super` path segments.
                                visible_path.root = res::DefPathRootKind::Parent { supers: super_mods.len() };
                                return visible_path;
                            }
                        }
                        DefPathRequestKind::ParentModStub(_) => {
                            // Construct direct path of `super` path segments, which is not valid by itself,
                            // but will be valid once the caller appends the item segment(s) to it.
                            return res::DefPath::new(res::DefPathRootKind::Parent { supers: super_mods.len() }, vec![]);
                        }
                    }
                }
            }
        }

        if let Some(visible_path) = res::visible_def_path(self.tcx, self.crate_res, def_id, self.current_scope, ignore_reexport, span) {
            return visible_path;
        }

        // Ensure that the def is in the current scope, otherwise it really is not visible from here.
        let Some(current_scope) = self.current_scope else {
            span_bug!(span, "{} is not accessible in this crate", self.tcx.def_path_str(def_id));
        };
        match res::locally_visible_def_path(self.tcx, def_id, current_scope) {
            Ok(visible_path) => { return visible_path; }
            Err(adjusted_scope) => {
                span_bug!(span, "{def} is not defined in the scope {scope} and is not otherwise accessible here",
                    def = self.tcx.def_path_str(def_id),
                    scope = self.tcx.def_path_str(adjusted_scope),
                );
            }
        }
    }

    #[track_caller]
    fn bug_unmatched_ast_node<S, F>(&self, span: Span, msg: F) -> !
    where
        S: Into<String>,
        F: FnOnce() -> S,
    {
        let mut diagnostic = self.tcx.dcx().struct_span_bug(span, msg().into());
        diagnostic.span_label(span, "no corresponding HIR node found");
        diagnostic.note(format!("body resolutions from {scope}",
            scope = match self.current_scope {
                Some(scope) =>  self.tcx.def_path_debug_str(scope),
                None => "<unknown scope>".to_owned(),
            },
        ));
        diagnostic.note(format!("expected at {}", std::panic::Location::caller()));
        diagnostic.emit();
    }

    #[must_use]
    fn adjust_path_from_expansion(&self, path: &mut ast::Path, res: hir::Res<ast::NodeId>, ignore_reexport: Option<hir::DefId>) -> Option<Ty<'tcx>> {
        match res {
            hir::Res::Local(node_id) => {
                let Some(hir_id) = self.body_res.hir_id(node_id) else {
                    self.bug_unmatched_ast_node(path.span, || format!("unable to resolve local `{}` for sanitization", ast::print::path_to_string(path)));
                };
                let def_ident = self.tcx.hir_ident(hir_id);

                let [ident_segment] = &mut path.segments[..] else { unreachable!() };
                copy_def_span_ctxt(&mut ident_segment.ident, def_ident.span);
                sanitize_ident_if_from_expansion(&mut ident_segment.ident, IdentResKind::Local);

                None
            }

            hir::Res::Def(def_kind, mut def_id) => {
                match def_kind {
                    | hir::DefKind::Mod
                    | hir::DefKind::Struct
                    | hir::DefKind::Union
                    | hir::DefKind::Enum
                    | hir::DefKind::Variant
                    | hir::DefKind::Trait
                    | hir::DefKind::TyAlias { .. }
                    | hir::DefKind::TraitAlias
                    | hir::DefKind::ForeignMod
                    | hir::DefKind::ForeignTy
                    | hir::DefKind::Fn
                    | hir::DefKind::Const
                    | hir::DefKind::Static { .. }
                    | hir::DefKind::Ctor(..)
                    | hir::DefKind::AssocTy
                    | hir::DefKind::AssocFn
                    | hir::DefKind::AssocConst
                    | hir::DefKind::Macro(..)
                    => {
                        if let hir::DefKind::Ctor(..) = def_kind {
                            // Adjust target definition to the parent to avoid naming the unnamed constructors.
                            def_id = self.tcx.parent(def_id);
                        }

                        let visible_def_path = self.expect_visible_def_path(DefPathRequestKind::Item(def_id), path.span, ignore_reexport);
                        self.overwrite_path_with_def_path(path, &visible_def_path)
                    }

                    | hir::DefKind::TyParam
                    | hir::DefKind::LifetimeParam
                    | hir::DefKind::ConstParam
                    => {
                        let [param_segment] = &mut path.segments[..] else { unreachable!() };
                        sanitize_ident_if_from_expansion(&mut param_segment.ident, IdentResKind::Def);

                        None
                    }

                    hir::DefKind::Field => {
                        // TODO
                        None
                    }

                    | hir::DefKind::ExternCrate
                    | hir::DefKind::Use
                    | hir::DefKind::AnonConst
                    | hir::DefKind::InlineConst
                    | hir::DefKind::OpaqueTy
                    | hir::DefKind::GlobalAsm
                    | hir::DefKind::Impl { .. }
                    | hir::DefKind::Closure
                    | hir::DefKind::SyntheticCoroutineBody
                    => None
                }
            }

            | hir::Res::PrimTy(..)
            | hir::Res::SelfTyParam { .. }
            | hir::Res::SelfTyAlias { .. }
            | hir::Res::SelfCtor(..)
            | hir::Res::ToolMod
            | hir::Res::NonMacroAttr(..)
            | hir::Res::Err
            => None
        }
    }

    #[must_use]
    fn sanitize_path(&mut self, path: &mut ast::Path, res: hir::Res<ast::NodeId>, ignore_reexport: Option<hir::DefId>) -> Option<Ty<'tcx>> {
        // NOTE: We explicitly only visit the generic arguments, as we will
        //       sanitize the ident segments afterwards.
        for segment in &mut path.segments {
            if let Some(args) = &mut segment.args {
                self.visit_generic_args(args);
            }
        }

        // Short-circuit if `self`.
        if let [path_segment] = &path.segments[..] && path_segment.ident.name == kw::SelfLower { return None; }

        self.adjust_path_from_expansion(path, res, ignore_reexport)
    }

    fn typeck_for(&self, owner_id: hir::OwnerId) -> Option<&'tcx ty::TypeckResults<'tcx>> {
        self.current_typeck_ctx.filter(|typeck| typeck.hir_owner == owner_id)
    }

    fn lookup_hir_node_ty(&self, ty_hir: &hir::Ty<'tcx>) -> Ty<'tcx> {
        if let Some(typeck) = self.typeck_for(ty_hir.hir_id.owner) {
            if let Some(ty) = typeck.node_type_opt(ty_hir.hir_id) {
                return ty;
            }
        }

        // HACK: `ItemCtxt` and its `lower_ty` method are no longer public,
        //       so we have to use the only remaining accessible workaround,
        //       even though it is marked as "quasi-deprecated".
        rustc_hir_analysis::lower_ty(self.tcx, ty_hir)
    }

    fn sanitize_region(&self, region: ty::Region<'tcx>, binding_item_def_id: hir::DefId) -> Option<ast::Lifetime> {
        let span = match region.opt_param_def_id(self.tcx, binding_item_def_id) {
            Some(def_id) => self.tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP),
            None => DUMMY_SP,
        };
        let mut ident = Ident::new(region.get_name()?, span);
        sanitize_ident_if_from_expansion(&mut ident, IdentResKind::Def);

        Some(ast::mk::lifetime(DUMMY_SP, ident))
    }

    fn sanitize_ty(&self, ty: Ty<'tcx>, binding_item_def_id: hir::DefId, span: Span) -> P<ast::Ty> {
        let def_path_handling = ty::print::DefPathHandling::PreferVisible(ty::print::ScopedItemPaths::Trimmed);
        let opaque_ty_handling = ty::print::OpaqueTyHandling::Infer;
        let Some(ty_ast) = ty::ast_repr(self.tcx, self.crate_res, self.def_res, self.current_scope, span, ty, def_path_handling, opaque_ty_handling, true, binding_item_def_id) else {
            span_bug!(span, "cannot construct AST representation of type `{ty:?}`");
        };

        ty_ast
    }

    fn sanitize_generic_args(&self, generic_args: &[ty::GenericArg<'tcx>], binding_item_def_id: hir::DefId, span: Span) -> Option<P<ast::GenericArgs>> {
        let args_ast = generic_args.into_iter()
            .filter_map(|generic_arg| {
                match generic_arg.kind() {
                    ty::GenericArgKind::Lifetime(region) => {
                        let lifetime = self.sanitize_region(region, binding_item_def_id)?;
                        Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Lifetime(lifetime)))
                    }
                    ty::GenericArgKind::Type(ty) => {
                        let ty_ast = self.sanitize_ty(ty, binding_item_def_id, span);
                        Some(ast::AngleBracketedArg::Arg(ast::GenericArg::Type(ty_ast)))
                    }
                    ty::GenericArgKind::Const(_) => {
                        None // TODO
                    }
                }
            })
            .collect::<ThinVec<_>>();

        if args_ast.is_empty() { return None; }

        Some(P(ast::GenericArgs::AngleBracketed(ast::AngleBracketedArgs { span: DUMMY_SP, args: args_ast })))
    }

    /// Extract bound params from local trait bounds corresponding to the parameter,
    /// which must be appended to the trait subpath if the parameter is in a qualified self position:
    /// `<T as Trait<'a, 'b>>::$assoc where T: Trait<'a, 'b>`.
    fn extract_local_trait_bound_params(&self, trait_def_id: hir::DefId, param_res: hir::Res<ast::NodeId>, span: Span) -> Option<P<ast::GenericArgs>> {
        let (parent_def_id, generic_predicates, param_index) = match param_res {
            hir::Res::SelfTyAlias { alias_to: impl_def_id, is_trait_impl: true, .. } => {
                let Some(trait_ref) = self.tcx.impl_trait_ref(impl_def_id) else { unreachable!() };

                // We instantiate the trait predicates with the impl's trait arguments
                // for correct resolution of the local bounds later
                // (e.g. if a lifetime is bound with a different name in the impl than in the trait).
                // NOTE: To easily find the `Self` predicates later using the 0 parameter index,
                //       we replace the concrete `Self` argument with the generic `Self` parameter type
                //       that appears in the trait def.
                let dummy_self_param_ty = ty::ParamTy::new(0, kw::SelfUpper).to_ty(self.tcx);
                let args = self.tcx.mk_args_trait(dummy_self_param_ty, trait_ref.skip_binder().args.iter().skip(1));
                let generic_predicates = self.tcx.predicates_of(trait_ref.skip_binder().def_id).instantiate(self.tcx, args);

                (impl_def_id, generic_predicates, 0)
            }
            | hir::Res::SelfTyAlias { alias_to: trait_def_id, is_trait_impl: false, .. }
            | hir::Res::SelfTyParam { trait_: trait_def_id } => {
                let generic_predicates = self.tcx.predicates_of(trait_def_id).instantiate_identity(self.tcx);
                (trait_def_id, generic_predicates, 0)
            }
            hir::Res::Def(hir::DefKind::TyParam, param_def_id) => {
                let generics = self.tcx.generics_of(self.tcx.parent(param_def_id));
                let Some(&param_index) = generics.param_def_id_to_index.get(&param_def_id) else { unreachable!() };
                let generic_predicates = self.tcx.predicates_of(self.tcx.parent(param_def_id)).instantiate_identity(self.tcx);
                (self.tcx.parent(param_def_id), generic_predicates, param_index)
            }
            _ => { return None; }
        };

        let predicates = generic_predicates.predicates.iter()
            .filter_map(|&clause| clause.as_trait_clause().map(|p| p.skip_binder()))
            .filter(|trait_predicate| {
                let ty::TyKind::Param(param_ty) = trait_predicate.self_ty().kind() else { return false; };
                param_ty.index == param_index
            })
            .collect::<Vec<_>>();

        let Some(trait_predicate) = predicates.iter().find(|trait_predicate| trait_predicate.def_id() == trait_def_id) else {
            // No trait predicates for the trait, skip.
            return None;
        };

        self.sanitize_generic_args(&trait_predicate.trait_ref.args[1..], parent_def_id, span)
    }

    fn sanitize_qualified_path(&mut self, qself: &mut Option<P<ast::QSelf>>, path: &mut ast::Path, node_id: ast::NodeId) -> hir::Res<ast::NodeId> {
        // Short-circuit if `self`.
        if let [path_segment] = &path.segments[..] && path_segment.ident.name == kw::SelfLower { return hir::Res::Err; }

        match (qself, self.def_res.node_res(node_id)) {
            // If the path can be resolved without type-checking, then it will be handled like in `visit_path`.
            (None, Some(res)) => {
                let None = self.sanitize_path(path, res, None) else {
                    span_bug!(path.span, "produced unexpected type-relative path for path with simple resolution");
                };
                res
            }

            // Otherwise, resolve it with the help of the corresponding HIR QPath.
            (qself @ _, _) => {
                let Some(node_hir_id) = self.body_res.hir_id(node_id) else {
                    self.bug_unmatched_ast_node(path.span, || format!("unable to resolve path `{}` for sanitization", ast::print::qpath_to_string(qself.as_deref(), path)));
                };

                let Some(qpath_hir) = self.tcx.hir_node(node_hir_id).qpath() else { span_bug!(path.span, "no corresponding qualified path in HIR") };

                match qpath_hir {
                    // NOTE: This corresponds to the already handled case where
                    //       a non-qualified path has a concrete AST resolution.
                    hir::QPath::Resolved(None, _) => span_bug!(path.span, "encountered unexpected resolved, non-qualified HIR path"),

                    | hir::QPath::Resolved(Some(qself_ty_hir), _)
                    | hir::QPath::TypeRelative(qself_ty_hir, _) => {
                        let mut qres = match self.typeck_for(node_hir_id.owner) {
                            Some(typeck) => typeck.qpath_res(&qpath_hir, node_hir_id),
                            None => {
                                match qpath_hir {
                                    hir::QPath::Resolved(_, path_hir) => path_hir.res,
                                    _ => hir::Res::Err,
                                }
                            }
                        };
                        if let hir::Res::Err = qres {
                            'fixup: {
                                let node_hir = self.tcx.hir_node(node_hir_id);

                                // NOTE: In the case of expression patterns, the type dependent def
                                //       is associated with the pattern expression node, not the pattern node itself.
                                if let hir::Node::Pat(pat_hir) = node_hir && let hir::PatKind::Expr(pat_expr_hir) = &pat_hir.kind {
                                    // NOTE: Patterns can only be found in bodies.
                                    let Some(typeck) = self.typeck_for(node_hir_id.owner) else { unreachable!() };
                                    qres = typeck.qpath_res(&qpath_hir, pat_expr_hir.hir_id);
                                }

                                if !matches!(qres, hir::Res::Err) { break 'fixup; }

                                match node_hir {
                                    hir::Node::Ty(ty_hir) => {
                                        // HACK: `ItemCtxt` and its `lower_ty` method are no longer public,
                                        //       so we have to use the only remaining accessible workaround,
                                        //       even though it is marked as "quasi-deprecated".
                                        let ty = rustc_hir_analysis::lower_ty(self.tcx, ty_hir);
                                        let ty::TyKind::Alias(ty::AliasTyKind::Projection, alias_ty) = ty.kind() else { unreachable!() };

                                        let trait_item_def_id = alias_ty.def_id;
                                        let trait_item_def_kind = self.tcx.def_kind(alias_ty.def_id);

                                        qres = hir::Res::Def(trait_item_def_kind, trait_item_def_id);
                                    }

                                    _ => span_bug!(path.span, "path `{}` cannot be resolved", ast::print::qpath_to_string(qself.as_deref(), path)),
                                }
                            }
                        }

                        let qself_ty = self.lookup_hir_node_ty(qself_ty_hir);

                        // NOTE: All nested qpaths can be reduced down to a simply qualified path by resolving the definition.
                        let parent_def_id = self.tcx.parent(qres.def_id());
                        match self.tcx.def_kind(parent_def_id) {
                            hir::DefKind::Trait | hir::DefKind::TraitAlias => {
                                let qself_ty_ast = self.sanitize_ty(qself_ty, node_hir_id.owner.to_def_id(), qself_ty_hir.span);

                                let parent_path_segment_res = match &path.segments[..] {
                                    [.., parent_path_segment, _] => self.def_res.node_res(parent_path_segment.id),
                                    _ => None,
                                };

                                // NOTE: We always add a qualified self type, so we can safely ignore the indicator return value.
                                let _ = self.sanitize_path(path, qres.expect_non_local(), None);

                                // Extract param args for the trait reference in the qualified path.
                                // These param args come from type-inference, if available, or
                                // from bound params in local trait bounds in the current scope.
                                let generic_args_ast = match self.typeck_for(node_hir_id.owner).map(|typeck| &typeck.node_args(node_hir_id)[..]) {
                                    // Inferred generic args for the trait.
                                    Some(node_args @ [_, ..]) => {
                                        let trait_generics = self.tcx.generics_of(parent_def_id);
                                        let trait_args = &node_args[(trait_generics.has_self as usize)..trait_generics.count()];

                                        self.sanitize_generic_args(trait_args, node_hir_id.owner.to_def_id(), qself_ty_hir.span)
                                    }

                                    // Bound params from local trait bounds corresponding to parameter types to the trait subpath.
                                    _ if let Some(parent_path_segment_res) = parent_path_segment_res => {
                                        self.extract_local_trait_bound_params(parent_def_id, parent_path_segment_res, qself_ty_hir.span)
                                    }

                                    _ => None,
                                };
                                // Append param args to the trait reference in the qualified path.
                                if let Some(generic_args_ast) = generic_args_ast {
                                    let [.., parent_path_segment, _] = &mut path.segments[..] else { unreachable!() };
                                    parent_path_segment.args = Some(generic_args_ast);
                                }

                                *qself = Some(P(ast::QSelf {
                                    ty: qself_ty_ast,
                                    path_span: DUMMY_SP,
                                    position: path.segments.len() - 1,
                                }));

                                // NOTE: For field lookups, we keep using the Res after path resolution.
                                //       To do so, we must reformulate resolutions to trait associated types
                                //       to incorporate the definition's Self type.
                                if let hir::Res::Def(hir::DefKind::AssocTy, trait_item_def_id) = qres {
                                    // NOTE: We only need to do this if we are in a body, which means that
                                    //       typecheck results must be available.
                                    let Some(typeck) = self.typeck_for(node_hir_id.owner) else { return qres.expect_non_local(); };

                                    match parent_path_segment_res {
                                        None => unreachable!(),

                                        // `T:Assoc` items cannot be directly resolved to any one impl assoc item.
                                        Some(hir::Res::Def(hir::DefKind::TyParam, _)) => {}

                                        // Qualifed path with explicit trait qualification.
                                        Some(hir::Res::Def(hir::DefKind::Trait, _)) => {
                                            // let user_provided_ty = typeck.user_provided_types().get(node_hir_id);
                                            // println!("  user_provided_ty = {user_provided_ty:?}");

                                            let Some(canonical_user_ty) = typeck.user_provided_types().get(node_hir_id) else {
                                                // NOTE: This only happens if the path is not in a body.
                                                //       In this case, we do not need to make adjustments to the res
                                                //       as it will not be used, see above.
                                                return qres.expect_non_local();
                                            };

                                            // Retrieve explicit arguments to trait.
                                            let user_args = match canonical_user_ty.value.kind {
                                                ty::UserTypeKind::TypeOf(_, user_args) => user_args,

                                                // Ignore type annotations (i.e. `: $ty`), as their res is never used, see above.
                                                ty::UserTypeKind::Ty(_) => { return qres.expect_non_local(); }
                                            };
                                            let Some(user_self_ty) = user_args.user_self_ty else { unreachable!() };
                                            let ty::TyKind::Alias(_, alias_ty) = user_self_ty.self_ty.kind() else { unreachable!() };

                                            let assoc_item_trait_def_id = self.tcx.parent(trait_item_def_id);

                                            let assoc_item_trait_ref = ty::TraitRef::new(self.tcx, assoc_item_trait_def_id, alias_ty.args);
                                            let assoc_item_trait_predicate = ty::TraitPredicate { trait_ref: assoc_item_trait_ref, polarity: ty::PredicatePolarity::Positive };

                                            let param_env = self.tcx.param_env(node_hir_id.owner.to_def_id());
                                            let infcx = self.tcx.infer_ctxt().build(TypingMode::PostAnalysis);
                                            let mut selcx = SelectionContext::new(&infcx);
                                            let Ok(Some(ImplSource::UserDefined(data))) = selcx.select(&Obligation::new(self.tcx, ObligationCause::dummy(), param_env, assoc_item_trait_predicate)) else {
                                                span_bug!(path.span, "cannot resolve impl for `{qself_ty}` of the trait of the associated item {}", self.tcx.def_path_str(trait_item_def_id))
                                            };
                                            let assoc_item_impl_def_id = data.impl_def_id;

                                            let Some(&impl_item_def_id) = self.tcx.impl_item_implementor_ids(assoc_item_impl_def_id).get(&trait_item_def_id) else { unreachable!() };

                                            return hir::Res::Def(hir::DefKind::AssocTy, impl_item_def_id);
                                        }

                                        // Reference to a trait item; leave as-is.
                                        Some(hir::Res::SelfTyParam { trait_: _ }) => {}

                                        // Impossible case for trait items: cannot refer to trait associated type through `Self`
                                        // without fully qualified syntax in an inherent impl context.
                                        // See `tests/ui/hygiene/rustc_res/implicit_trait_assoc_ty_not_available_in_inherent_impl`.
                                        // Possible for inherent impl items, but in that case the res already points to the impl's assoc item.
                                        Some(hir::Res::SelfTyAlias { alias_to: _trait_def_id, is_trait_impl: false, .. }) => {}

                                        // `Self::Assoc` paths.
                                        // NOTE: We have to resolve the trait impl through the generic predicates in context,
                                        //       as the trait arguments are inferred in this case.
                                        Some(hir::Res::SelfTyAlias { alias_to: impl_def_id, is_trait_impl: true, .. }) => {
                                            if let Some(&impl_item_def_id) = self.tcx.impl_item_implementor_ids(impl_def_id).get(&trait_item_def_id) {
                                                return hir::Res::Def(hir::DefKind::AssocTy, impl_item_def_id);
                                            }

                                            let assoc_item_trait_def_id = self.tcx.parent(trait_item_def_id);

                                            let Some(trait_ref) = self.tcx.impl_trait_ref(impl_def_id) else { unreachable!() };
                                            let generic_predicates = self.tcx.predicates_of(trait_ref.skip_binder().def_id).instantiate(self.tcx, trait_ref.skip_binder().args);

                                            // Extract impl predicate related to the trait of the assoc item.
                                            let Some(assoc_item_trait_predicate) = generic_predicates.predicates.iter()
                                                .filter_map(|&clause| clause.as_trait_clause().map(|p| p.skip_binder()))
                                                .find(|trait_predicate| {
                                                    trait_predicate.trait_ref.def_id == assoc_item_trait_def_id
                                                        && trait_predicate.self_ty() == trait_ref.skip_binder().self_ty()
                                                })
                                            else { span_bug!(path.span, "cannot find trait predicate related to the trait of associated item {}", self.tcx.def_path_str(trait_item_def_id)) };

                                            let param_env = self.tcx.param_env(impl_def_id);
                                            let infcx = self.tcx.infer_ctxt().build(TypingMode::PostAnalysis);
                                            let mut selcx = SelectionContext::new(&infcx);
                                            let Ok(Some(ImplSource::UserDefined(data))) = selcx.select(&Obligation::new(self.tcx, ObligationCause::dummy(), param_env, assoc_item_trait_predicate)) else {
                                                span_bug!(path.span, "cannot resolve impl for `Self` of the trait of the associated item {}", self.tcx.def_path_str(trait_item_def_id))
                                            };
                                            let assoc_item_impl_def_id = data.impl_def_id;

                                            let Some(&impl_item_def_id) = self.tcx.impl_item_implementor_ids(assoc_item_impl_def_id).get(&trait_item_def_id) else { unreachable!() };

                                            return hir::Res::Def(hir::DefKind::AssocTy, impl_item_def_id);
                                        }

                                        _ => span_bug!(path.span, "unhandled associated item root resolution: {parent_path_segment_res:?}"),
                                    }
                                }
                            }

                            hir::DefKind::Impl { of_trait: false } => {
                                let qself_ty_ast = self.sanitize_ty(qself_ty, node_hir_id.owner.to_def_id(), qself_ty_hir.span);

                                // NOTE: We always add a qualified self type, so we can safely ignore the indicator return value.
                                let _ = self.sanitize_path(path, qres.expect_non_local(), None);

                                // Remove the non-assoc part of the path and replace it with
                                // a "dummy" qualified self using the type derived from the type system
                                // corresponding to the HIR self type, like so:
                                // `<$ty>::$assoc` (note the lack of the `as` qualifier).
                                path.segments.splice(0..(path.segments.len() - 1), []);
                                *qself = Some(P(ast::QSelf {
                                    ty: qself_ty_ast,
                                    path_span: DUMMY_SP,
                                    position: 0,
                                }));
                            }

                            // If the portion of the path within the qself does not refer to a trait or impl,
                            // then the resolved path no longer needs a qualified self.
                            _ => {
                                let None = self.sanitize_path(path, qres.expect_non_local(), None) else {
                                    span_bug!(path.span, "produced unexpected type-relative path for non-assoc path")
                                };
                                *qself = None;
                            }
                        }

                        qres.expect_non_local()
                    }

                    hir::QPath::LangItem(_, _) => span_bug!(path.span, "encountered #[lang] item path"),
                }
            }
        }
    }

    fn sanitize_import_path(&mut self, scope: hir::HirId, import_node_id: ast::NodeId, path: &mut ast::Path, res: hir::Res<ast::NodeId>) {
        enum ModPathKind {
            DirectPath,
            ParentModPathStub,
        }

        let adjust_mod_path_from_expansion = |path: &mut ast::Path, mod_def_id: hir::ModDefId, mod_path_kind: ModPathKind, ignore_reexport: Option<hir::DefId>| {
            let def_path_request = match mod_path_kind {
                ModPathKind::DirectPath => DefPathRequestKind::Item(mod_def_id.to_def_id()),
                ModPathKind::ParentModPathStub => DefPathRequestKind::ParentModStub(mod_def_id),
            };

            let visible_def_path = self.expect_visible_def_path(def_path_request, path.span, ignore_reexport);
            let None = self.overwrite_path_with_def_path(path, &visible_def_path) else {
                span_bug!(path.span, "produced type-relative path in context which disallows qualified paths");
            };
        };

        let Some(&import_def_id) = self.def_res.node_id_to_def_id.get(&import_node_id) else { unreachable!() };

        match res {
            // If the whole import path (besides any glob suffix) points to a module, then the resolution is much more simple,
            // and we can simply sanitize the whole path in one go.
            hir::Res::Def(hir::DefKind::Mod, def_id) => 'arm: {
                // NOTE: Module re-exports (e.g. `pub use alloc::vec` in `std`) point to the underlying module, but
                //       we want to refer to the re-export by path in case the import this resolution comes from
                //       is diverging and points to an identically named item in the re-export's scope.
                // Check if the parent module named in the path diverges from the final resolution's parent module,
                // indicating that we are dealing with a module re-export resolution.
                if let [.., parent_mod_path_segment, _] = &path.segments[..]
                    && let Some(hir::Res::Def(hir::DefKind::Mod, parent_mod_def_id)) = self.def_res.node_res(parent_mod_path_segment.id)
                    && parent_mod_def_id != self.tcx.parent(def_id)
                {
                    // HACK: Deal with module re-export paths like we do with item paths.
                    break 'arm;
                }

                let mod_def_id = hir::ModDefId::new_unchecked(def_id);
                return adjust_mod_path_from_expansion(path, mod_def_id, ModPathKind::DirectPath, Some(import_def_id.to_def_id()));
            }

            hir::Res::Def(_, _) => {}

            hir::Res::PrimTy(_) => {
                // NOTE: Primitive types can be imported and even aliased.
                //       They can take two forms, `$ty` and `::{std/core}::primitive::$ty`, but
                //       there is no need to change either form in any context
                //       (when it has already been validated by the compiler), so
                //       we can keep these paths as-is.
                return;
            }

            _ => span_bug!(path.span, "import path has non-def last segment"),
        }

        let (mod_path_segments, item_path_segment, item_res, enum_variant) = match &mut path.segments[..] {
            [mod_path_segments @ .., item_path_segment, enum_variant_path_segment]
                if let Some(item_res @ hir::Res::Def(hir::DefKind::Enum, _)) = self.def_res.node_res(item_path_segment.id)
            => (mod_path_segments, item_path_segment, item_res, Some((enum_variant_path_segment, res))),

            [mod_path_segments @ .., item_path_segment] => (mod_path_segments, item_path_segment, res, None),

            _ => span_bug!(path.span, "empty import path"),
        };
        let hir::Res::Def(_, item_def_id) = item_res else { span_bug!(path.span, "import path has non-def item segment") };

        let mod_scope = self.tcx.parent_module(scope).to_def_id();
        let overlay_mod_scope = match self.macros_2_0_top_level_relative_path_res_hack {
            Macros2_0TopLevelRelativePathResHack::InTopLevelMacros2_0Scope { parent_module: macros_2_0_def_parent_mod } => Some(macros_2_0_def_parent_mod),
            _ => None,
        };

        let (parent_mod_def_id, referenced_mod_child) = match mod_path_segments {
            [.., parent_mod_path_segment] if let Some(parent_mod_res) = self.def_res.node_res(parent_mod_path_segment.id) => {
                let hir::Res::Def(hir::DefKind::Mod, parent_mod_def_id) = parent_mod_res else { span_bug!(path.span, "import path has non-mod prefix segment") };
                let Some(referenced_mod_child) = res::lookup_mod_child(self.tcx, parent_mod_def_id, item_res.expect_non_local(), item_path_segment.ident.name) else {
                    span_bug!(path.span, "cannot resolve item {} in module {}", self.tcx.def_path_str(item_def_id), self.tcx.def_path_str(parent_mod_def_id))
                };
                (parent_mod_def_id, referenced_mod_child)
            }

            // `$crate::Item` paths.
            [dollar_crate_segment]  if dollar_crate_segment.ident.name == kw::DollarCrate => {
                let crate_num = dollar_crate_segment.ident.span.ctxt().outer_expn_data().macro_def_id.unwrap().krate;
                let Some(referenced_mod_child) = res::lookup_mod_child(self.tcx, crate_num.as_def_id(), item_res.expect_non_local(), item_path_segment.ident.name) else {
                    span_bug!(path.span, "cannot resolve item {} in module {}", self.tcx.def_path_str(item_def_id), self.tcx.def_path_str(crate_num.as_def_id()))
                };
                (crate_num.as_def_id(), referenced_mod_child)
            }

            // `crate::Item` paths.
            [crate_segment] if crate_segment.ident.name == kw::Crate => {
                // NOTE: It is important that we look into the local crate root first, to avoid potential visibility issues.
                let crate_scopes = [LOCAL_CRATE.as_def_id(), item_def_id.krate.as_def_id()];

                let Some((parent_mod_def_id, referenced_mod_child)) = crate_scopes.into_iter().find_map(|scope| {
                    let referenced_mod_child = res::lookup_mod_child(self.tcx, scope, item_res.expect_non_local(), item_path_segment.ident.name)?;
                    Some((scope, referenced_mod_child))
                }) else {
                    let searched_mods = crate_scopes.into_iter().map(|parent_mod_def_id| self.tcx.def_path_str(parent_mod_def_id));
                    span_bug!(path.span, "cannot resolve item {} in modules {}", self.tcx.def_path_str(item_def_id), searched_mods.intersperse(", ".to_owned()).collect::<String>())
                };

                (parent_mod_def_id, referenced_mod_child)
            }

            // `super{::super}*::Item` paths.
            [super_segments @ ..] if !super_segments.is_empty() && super_segments.iter().all(|segment| segment.ident.name == kw::Super) => {
                let mut parent_mod_def_id = overlay_mod_scope.unwrap_or(mod_scope);
                for _ in 0..super_segments.len() {
                    parent_mod_def_id = self.tcx.parent_module_from_def_id(parent_mod_def_id.expect_local()).to_def_id();
                }
                let Some(referenced_mod_child) = res::lookup_mod_child(self.tcx, parent_mod_def_id, item_res.expect_non_local(), item_path_segment.ident.name) else {
                    span_bug!(path.span, "cannot resolve item {} in module {}", self.tcx.def_path_str(item_def_id), self.tcx.def_path_str(parent_mod_def_id))
                };
                (parent_mod_def_id, referenced_mod_child)
            }

            // `{self::}?Item` paths.
            [] | [ast::PathSegment { ident: Ident { name: kw::SelfLower, .. }, .. }] => {
                let mod_scopes = [overlay_mod_scope, Some(mod_scope), self.prelude_mod];

                let Some((parent_mod_def_id, referenced_mod_child)) = mod_scopes.into_iter().flatten().find_map(|scope| {
                    let referenced_mod_child = res::lookup_mod_child(self.tcx, scope, item_res.expect_non_local(), item_path_segment.ident.name)?;
                    Some((scope, referenced_mod_child))
                }) else {
                    let searched_mods = mod_scopes.into_iter().flatten().map(|parent_mod_def_id| self.tcx.def_path_str(parent_mod_def_id));
                    span_bug!(path.span, "cannot resolve item {} in modules {}", self.tcx.def_path_str(item_def_id), searched_mods.intersperse(", ".to_owned()).collect::<String>())
                };

                (parent_mod_def_id, referenced_mod_child)
            }

            _ => span_bug!(path.span, "unhandled import path root with missing parent mod resolutions"),
        };

        let mod_child_path_segments_count = match enum_variant {
            None => 1,
            Some(_) => 2,
        };

        if item_def_id.is_local() {
            let mut def_ident = referenced_mod_child.ident;
            sanitize_ident_if_from_expansion(&mut def_ident, IdentResKind::Def);
            item_path_segment.ident = def_ident.with_span_pos(item_path_segment.ident.span);

            if let Some((enum_variant_path_segment, enum_variant_res)) = enum_variant {
                let hir::Res::Def(_, enum_variant_def_id) = enum_variant_res else { span_bug!(path.span, "import path has non-def enum child segment") };

                let Some(referenced_enum_child) = res::lookup_mod_child(self.tcx, item_def_id, enum_variant_res.expect_non_local(), enum_variant_path_segment.ident.name) else {
                    span_bug!(path.span, "cannot resolve item {} in enum {}", self.tcx.def_path_str(enum_variant_def_id), self.tcx.def_path_str(item_def_id))
                };

                let mut def_ident = referenced_enum_child.ident;
                sanitize_ident_if_from_expansion(&mut def_ident, IdentResKind::Def);
                enum_variant_path_segment.ident = def_ident.with_span_pos(enum_variant_path_segment.ident.span);
            }
        }

        if parent_mod_def_id == mod_scope {
            path.segments.splice(0..(path.segments.len() - mod_child_path_segments_count), []);
            return;
        }

        let mut parent_mod_path = ast::Path { span: DUMMY_SP, segments: thin_vec![], tokens: None };
        let parent_mod_def_id = hir::ModDefId::new_unchecked(parent_mod_def_id);
        adjust_mod_path_from_expansion(&mut parent_mod_path, parent_mod_def_id, ModPathKind::ParentModPathStub, Some(import_def_id.to_def_id()));
        path.segments.splice(0..(path.segments.len() - mod_child_path_segments_count), parent_mod_path.segments);
    }

    fn sanitize_use_tree(&mut self, use_tree: &mut ast::UseTree, node_id: ast::NodeId, span: Span) {
        use itertools::Itertools;

        enum UseKind {
            Glob,
            Single {
                /// Name originally introduced by this use import, before import path sanitization.
                /// Either the last path segment, or the name alias.
                ident: Ident,
                /// Namespace in which the name is introduced by this use import.
                namespace: hir::Namespace,
                rename: Option<Ident>,
            }
        }

        struct Import {
            res: hir::Res<ast::NodeId>,
            /// Path segments of the import item, assembled from the nested use tree prefixes that lead to this import item.
            path_segments: ThinVec<ast::PathSegment>,
            node_id: ast::NodeId,
            use_kind: UseKind,
        }

        fn extract_imports_from_use_tree<'tcx>(tcx: TyCtxt<'tcx>, def_res: &ast_lowering::DefResolutions, imports: &mut Vec<Import>, mut root_path_segments: ThinVec<ast::PathSegment>, use_tree: &ast::UseTree, node_id: ast::NodeId) {
            root_path_segments.extend(use_tree.prefix.segments.iter().cloned());
            let mut path_segments = root_path_segments;

            // NOTE: `*::self` paths are only valid when the self appears in a nested use tree, and
            //       refer to the same item as the path segment before.
            //       Since they complicate path segment resolution and printing of valid paths,
            //       it is easiest to just strip these no-op suffix segments here.
            if let [.., last_path_segment] = &path_segments[..] && last_path_segment.ident.name == kw::SelfLower {
                path_segments.pop();
            }

            match &use_tree.kind {
                ast::UseTreeKind::Glob => {
                    let Some(res) = def_res.node_res(node_id).or_else(|| {
                        let [.., last_prefix_segment] = &use_tree.prefix.segments[..] else { return None; };
                        def_res.node_res(last_prefix_segment.id)
                    }) else { span_bug!(path_segments.last().unwrap().ident.span, "import path cannot be resolved") };

                    imports.push(Import { res, path_segments, node_id, use_kind: UseKind::Glob });
                }

                ast::UseTreeKind::Simple(rename) => {
                    let Some(import_res) = def_res.import_res(node_id) else {
                        span_bug!(path_segments.last().unwrap().ident.span, "import path cannot be resolved")
                    };

                    let import_res_with_ns = [
                        (import_res.type_ns.map(|res| (hir::Namespace::TypeNS, res))),
                        (import_res.value_ns.map(|res| (hir::Namespace::ValueNS, res))),
                        (import_res.macro_ns.map(|res| (hir::Namespace::MacroNS, res))),
                    ].into_iter().flatten();

                    let ident = rename.unwrap_or_else(|| path_segments.last().unwrap().ident);
                    import_res_with_ns
                        .map(|(namespace, res)| Import { res, path_segments: path_segments.clone(), node_id, use_kind: UseKind::Single { ident, namespace, rename: *rename } })
                        .collect_into(imports);
                }

                ast::UseTreeKind::Nested { items, span: _ } => {
                    for (nested_use_tree, inner_node_id) in items {
                        extract_imports_from_use_tree(tcx, def_res, imports, path_segments.clone(), nested_use_tree, *inner_node_id);
                    }
                }
            }
        }

        let Some(&scope_def_id) = self.def_res.node_id_to_def_id.get(&node_id) else { unreachable!() };
        let scope = self.tcx.local_def_id_to_hir_id(scope_def_id);

        let mut imports = vec![];
        extract_imports_from_use_tree(self.tcx, self.def_res, &mut imports, thin_vec![], &use_tree, node_id);

        let import_paths = imports.into_iter()
            .map(|import| {
                let span = import.path_segments.last().unwrap().ident.span;
                let mut path = ast::Path { span, segments: import.path_segments, tokens: None };
                self.sanitize_import_path(scope, import.node_id, &mut path, import.res);

                let mut use_kind = import.use_kind;
                if let UseKind::Single { ident, rename, .. } = &mut use_kind {
                    // Retain name introduced by the original import, but sanitize the name.
                    match rename {
                        Some(rename) => sanitize_ident_if_from_expansion(rename, IdentResKind::Def),
                        None => {
                            let [.., last_path_segment] = &path.segments[..] else { unreachable!() };
                            sanitize_ident_if_from_expansion(ident, IdentResKind::Def);
                            if last_path_segment.ident.name != ident.name {
                                *rename = Some(*ident);
                            }
                        }
                    }
                }

                (path, use_kind)
            })
            // Dedupe imports based on idents introduced into namespaces.
            .dedup_by(|a, b| {
                match (a, b) {
                    // NOTE: Dublicate glob imports of the same module are already invalid before sanitization.
                    ((_, UseKind::Glob), (_, UseKind::Glob)) => false,

                    ((path, UseKind::Single { ident, namespace, .. }), (next_path, UseKind::Single{ ident: next_ident, namespace: next_namespace, .. })) => {
                        #[inline]
                        fn matching_paths(path: &ast::Path, next_path: &ast::Path) -> bool {
                            path.segments.len() == next_path.segments.len()
                                && iter::zip(&path.segments, &next_path.segments).all(|(segment, next_segment)| segment.ident.name == next_segment.ident.name)
                        }

                        // NOTE: If we encounter the same name relating to two different namespaces,
                        //       then the paths must be different, otherwise
                        //       the two imports will refer to the same names in the same namespaces.
                        ident == next_ident && (namespace == next_namespace || matching_paths(path, next_path))
                    }

                    _ => false,
                }
            });

        // Convert the top-level use tree into a prefixless group of resolved import paths,
        // i.e. `use {crate::$path::$to::$item, $relative::$path::$to::$local::$item, ...}`.
        use_tree.prefix.segments = thin_vec![];
        let use_tree_items = import_paths
            .map(|(path, use_kind)| {
                let nested_use_tree = ast::UseTree {
                    prefix: path,
                    kind: match use_kind {
                        UseKind::Glob => ast::UseTreeKind::Glob,
                        UseKind::Single { rename, .. } => ast::UseTreeKind::Simple(rename),
                    },
                    span: DUMMY_SP,
                };

                (nested_use_tree, ast::DUMMY_NODE_ID)
            })
            .collect::<ThinVec<_>>();
        use_tree.kind = ast::UseTreeKind::Nested { items: use_tree_items, span };
    }
}

pub fn sanitize_path<'tcx>(tcx: TyCtxt<'tcx>, crate_res: &res::CrateResolutions<'tcx>, def_res: &ast_lowering::DefResolutions, scope: Option<hir::DefId>, path: &mut ast::Path, res: hir::Res<ast::NodeId>, descend_into_args: bool) {
    let mut sanitizer = MacroExpansionSanitizer {
        tcx,
        crate_res,
        def_res,
        body_res: &ast_lowering::BodyResolutions::empty(tcx),
        syntax_extensions: vec![],
        // NOTE: This is only used for sanitizing import paths, which is the only reason we can ignore setting it here.
        prelude_mod: None,
        current_scope: scope,
        current_typeck_ctx: None,
        macros_2_0_top_level_relative_path_res_hack: Macros2_0TopLevelRelativePathResHack::NotInMacros2_0Scope,
        protected_idents: Default::default(),
    };

    if descend_into_args {
        // NOTE: We explicitly only visit the generic arguments, as we will
        //       sanitize the ident segments afterwards.
        for segment in &mut path.segments {
            if let Some(args) = &mut segment.args {
                sanitizer.visit_generic_args(args);
            }
        }
    }

    let None = sanitizer.adjust_path_from_expansion(path, res, None) else {
        span_bug!(path.span, "produced type-relative path in context which disallows qualified paths");
    };
}

macro def_flat_map_item_fns(
    $(fn $ident:ident(&mut $self:ident, $item:ident: $item_kind:ident $(,$($args:tt)*)?) |$def_id:ident| {
        walk_item: $walk_item:expr,
        $(check_item $check_item:block)?
        $(check_def $check_def:block)?
    });+;
) {
    $(
        fn $ident(&mut $self, mut $item: P<ast::Item<ast::$item_kind>> $(, $($args)*)?) -> SmallVec<[P<ast::Item<ast::$item_kind>>; 1]> {
            // Skip generated items corresponding to compiler (and mutest-rs) internals.
            if $item.id == ast::DUMMY_NODE_ID || $item.span == DUMMY_SP { return smallvec![$item]; }

            $($check_item)?

            let Some(&$def_id) = $self.def_res.node_id_to_def_id.get(&$item.id) else { unreachable!() };

            $($check_def)?

            // Store new context scope.
            let previous_scope = mem::replace(&mut $self.current_scope, Some($def_id.to_def_id()));

            // Store new typeck scope, if needed.
            let previous_typeck_ctx = $self.current_typeck_ctx;
            let typeck_root_def_id = $self.tcx.typeck_root_def_id($def_id.to_def_id());
            let Some(typeck_root_local_def_id) = typeck_root_def_id.as_local() else { unreachable!() };
            if let Some(typeck_root_body_id) = $self.tcx.hir_node_by_def_id(typeck_root_local_def_id).body_id() {
                if !previous_typeck_ctx.is_some_and(|previous_typeck_ctx| typeck_root_def_id == previous_typeck_ctx.hir_owner.to_def_id()) {
                    $self.current_typeck_ctx = Some($self.tcx.typeck_body(typeck_root_body_id));
                }
            }

            $walk_item;

            // Restore previous context.
            $self.current_typeck_ctx = previous_typeck_ctx;
            $self.current_scope = previous_scope;

            smallvec![$item]
        }
    )+
}

impl<'tcx, 'op> ast::mut_visit::MutVisitor for MacroExpansionSanitizer<'tcx, 'op> {
    def_flat_map_item_fns! {
        fn flat_map_item(&mut self, item: ItemKind) |def_id| {
            walk_item: ast::mut_visit::walk_item(self, &mut item),
            check_item {
                let id = item.id;
                let span = item.span;

                let macros_2_0_ctxt = item.span.ctxt().normalize_to_macros_2_0();
                let macros_2_0_expn_id = macros_2_0_ctxt.outer_expn();
                let macros_2_0_expn = macros_2_0_ctxt.outer_expn_data();
                if let ExpnKind::Macro(MacroKind::Bang, _) = macros_2_0_expn.kind {
                    match (self.macros_2_0_top_level_relative_path_res_hack, &item.kind) {
                        (Macros2_0TopLevelRelativePathResHack::InNestedMacros2_0Scope(expn_id), _) if expn_id == macros_2_0_expn_id => {}
                        (_, ast::ItemKind::Mod(_, _, _)) => {
                            self.macros_2_0_top_level_relative_path_res_hack = Macros2_0TopLevelRelativePathResHack::InNestedMacros2_0Scope(macros_2_0_expn_id);
                        }
                        _ => {
                            self.macros_2_0_top_level_relative_path_res_hack = Macros2_0TopLevelRelativePathResHack::InTopLevelMacros2_0Scope {
                                parent_module: macros_2_0_expn.parent_module.unwrap(),
                            };
                        }
                    }
                }

                if let ast::ItemKind::ExternCrate(symbol, ident) = &mut item.kind {
                    // Retain original crate name if not already aliased.
                    if symbol.is_none() { *symbol = Some(ident.name); }

                    // Sanitize local name of extern.
                    sanitize_ident_if_from_expansion(ident, IdentResKind::Def);

                    return smallvec![item];
                }

                if let ast::ItemKind::Use(use_tree) = &mut item.kind {
                    self.sanitize_use_tree(use_tree, id, span);
                    return smallvec![item];
                }
            }
        };

        fn flat_map_assoc_item(&mut self, item: AssocItemKind, assoc_ctxt: ast::visit::AssocCtxt) |def_id| {
            walk_item: ast::mut_visit::walk_assoc_item(self, &mut item, assoc_ctxt),
            check_def {
                let ident = match &mut item.kind {
                    ast::AssocItemKind::Const(const_item) => Some(&mut const_item.ident),
                    ast::AssocItemKind::Type(ty_alias) => Some(&mut ty_alias.ident),
                    ast::AssocItemKind::Fn(fn_item) => Some(&mut fn_item.ident),
                    ast::AssocItemKind::MacCall(_) => None,
                    ast::AssocItemKind::Delegation(delegation) => Some(&mut delegation.ident),
                    ast::AssocItemKind::DelegationMac(_) => None,
                };

                if let Some(ident) = ident {
                    // Match definition ident if this is an assoc item corresponding to a trait.
                    if let Some(assoc_item) = self.tcx.opt_associated_item(def_id.to_def_id()) {
                        match assoc_item.container {
                            ty::AssocItemContainer::Trait => {
                                // Trait items make new standalone definitions rather than referring to another definition,
                                // and so their ident does not have to be adjusted to another definition's.
                            }
                            ty::AssocItemContainer::Impl => {
                                // Adjustment only needed if this assoc item is in a trait impl, not a bare impl.
                                if let Some(trait_item_def_id) = assoc_item.trait_item_def_id {
                                    // HACK: Copy ident syntax context from trait item definition for correct sanitization later.
                                    let Some(trait_item_ident_span) = self.tcx.def_ident_span(trait_item_def_id) else { unreachable!() };
                                    copy_def_span_ctxt(ident, trait_item_ident_span);
                                }
                            }
                        }
                    }
                }
            }
        };
    }

    fn visit_attribute(&mut self, attr: &mut ast::Attribute) {
        // Some attributes correspond to already expanded derive proc macros, and thus need to be removed.
        if is_macro_helper_attr(&self.syntax_extensions, attr) {
            // Disable attribute by overriding it with an empty doc-comment.
            // This is easier than modifying every visit function to properly remove the attribute nodes.
            attr.kind = ast::AttrKind::DocComment(ast::token::CommentKind::Line, sym::empty)
        }
    }

    fn visit_ty(&mut self, ty: &mut P<ast::Ty>) {
        let ty_id = ty.id;

        match &mut ty.kind {
            ast::TyKind::Path(qself, path) => {
                let _res = self.sanitize_qualified_path(qself, path, ty_id);
                return;
            }
            _ => {}
        }

        ast::mut_visit::walk_ty(self, ty);
    }

    fn visit_assoc_item_constraint(&mut self, assoc_item_constraint: &mut ast::AssocItemConstraint) {
        // NOTE: We do not alter the idents of associated item constraints here.
        //       These get resolved in `adjust_path_from_expansion`.
        self.protected_idents.insert(assoc_item_constraint.ident);
        ast::mut_visit::walk_assoc_item_constraint(self, assoc_item_constraint);
        self.protected_idents.remove(&assoc_item_constraint.ident);
    }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        let mut protected_ident = None;

        let expr_id = expr.id;
        let expr_span = expr.span;

        match &mut expr.kind {
            ast::ExprKind::Path(qself, path) => {
                let _res = self.sanitize_qualified_path(qself, path, expr_id);
                return;
            }
            ast::ExprKind::Struct(struct_expr) => {
                let struct_expr = struct_expr.deref_mut();
                let res = self.sanitize_qualified_path(&mut struct_expr.qself, &mut struct_expr.path, expr_id);

                let variant_def = match res {
                    | hir::Res::SelfTyAlias { alias_to: alias_def_id, .. }
                    | hir::Res::Def(hir::DefKind::TyAlias, alias_def_id)
                    | hir::Res::Def(hir::DefKind::AssocTy, alias_def_id) => {
                        let self_ty = self.tcx.type_of(alias_def_id).instantiate_identity();
                        let ty::TyKind::Adt(adt_def, _) = self_ty.kind() else { unreachable!() };
                        adt_def.variant_of_res(res.expect_non_local())
                    }
                    // Expect a struct, union, or enum variant, and get the corresponding ADT variant.
                    _ => self.tcx.expect_variant_res(res.expect_non_local()),
                };

                for field in &mut struct_expr.fields {
                    let Some(field_def) = variant_def.fields.iter().find(|field_def| self.tcx.hygienic_eq(field.ident, field_def.ident(self.tcx), variant_def.def_id)) else {
                        span_bug!(field.span, "field `{}` does not match any field of {}", field.ident, self.tcx.def_path_str(variant_def.def_id));
                    };
                    // HACK: Copy ident syntax context from definition for correct sanitization later.
                    copy_def_span_ctxt(&mut field.ident, field_def.ident(self.tcx).span);
                    // NOTE: We have to disable shorthand syntax to ensure that
                    //       the correct field ident appears in printed code.
                    field.is_shorthand = false;
                }

                struct_expr.fields.flat_map_in_place(|field| self.flat_map_expr_field(field));

                match &mut struct_expr.rest {
                    ast::StructRest::Base(base_expr) => self.visit_expr(base_expr),
                    ast::StructRest::Rest(_span) => {}
                    ast::StructRest::None => {}
                }

                return;
            }
            ast::ExprKind::Field(base_expr, field_ident) => 'arm: {
                // Idents cannot start with a digit, therefore they must correspond
                // to an unnamed field reference which must not be sanitized.
                if field_ident.name.as_str().starts_with(|c: char| c.is_ascii_digit()) {
                    protected_ident = Some(*field_ident);
                    break 'arm;
                }

                let Some(base_expr_hir) = self.body_res.hir_expr(base_expr.peel_parens()) else {
                    self.bug_unmatched_ast_node(base_expr.span, || format!("unable to resolve field base expr `{}` for sanitization", ast::print::expr_to_string(expr)));
                };
                // HACK: The borrow checker does not allow for immutably referencing the expression for the `hir_expr` call
                //       because of the `&mut expr.kind` partial borrow above.
                let Some(expr_hir) = self.body_res.hir_node(expr_id).map(|hir_node| hir_node.expect_expr()) else {
                    self.bug_unmatched_ast_node(expr_span, || format!("unable to resolve field expr `{}` for sanitization", ast::print::expr_to_string(expr)));
                };

                let Some(typeck) = self.typeck_for(expr_hir.hir_id.owner) else { break 'arm; };

                let field_idx = typeck.field_index(expr_hir.hir_id);

                let base_ty = typeck.expr_ty_adjusted(base_expr_hir);
                let ty::TyKind::Adt(adt_def, _) = base_ty.kind() else { unreachable!() };
                let field_def = &adt_def.non_enum_variant().fields[field_idx];

                // HACK: Copy ident syntax context from definition for correct sanitization later.
                copy_def_span_ctxt(field_ident, field_def.ident(self.tcx).span);
            }
            ast::ExprKind::MethodCall(call) => {
                // HACK: The borrow checker does not allow for immutably referencing the expression for the `hir_expr` call
                //       because of the `&mut expr.kind` partial borrow above.
                let Some(expr_hir) = self.body_res.hir_node(expr_id).map(|hir_node| hir_node.expect_expr()) else {
                    self.bug_unmatched_ast_node(expr_span, || format!("unable to resolve method call expr `{}` for sanitization", ast::print::expr_to_string(expr)));
                };

                let Some(typeck) = self.typeck_for(expr_hir.hir_id.owner) else { unreachable!() };

                let Some(call_def_id) = typeck.type_dependent_def_id(expr_hir.hir_id) else {
                    span_bug!(expr_span, "cannot resolve type dependent def of method call")
                };

                // HACK: Copy ident syntax context from definition for correct sanitization later.
                let Some(call_item_ident_span) = self.tcx.def_ident_span(call_def_id) else { unreachable!() };
                copy_def_span_ctxt(&mut call.seg.ident, call_item_ident_span);
            }
            _ => {}
        }

        if let Some(protected_ident) = protected_ident { self.protected_idents.insert(protected_ident); }
        ast::mut_visit::walk_expr(self, expr);
        if let Some(protected_ident) = protected_ident { self.protected_idents.remove(&protected_ident); }
    }

    fn visit_pat(&mut self, pat: &mut P<ast::Pat>) {
        let mut protected_ident = None;

        let pat_id = pat.id;

        match &mut pat.kind {
            ast::PatKind::Ident(_, ident, _) => {
                if let Some(hir::Res::Local(..)) = self.def_res.node_res(pat_id) {
                    sanitize_standalone_ident_if_from_expansion(ident, IdentResKind::Local);
                    protected_ident = Some(*ident);
                }
            }
            ast::PatKind::Path(qself, path) => {
                let _res = self.sanitize_qualified_path(qself, path, pat_id);
                return;
            }
            ast::PatKind::Struct(qself, path, fields, _) => {
                let res = self.sanitize_qualified_path(qself, path, pat_id);

                let variant_def = match res {
                    | hir::Res::SelfTyAlias { alias_to: alias_def_id, .. }
                    | hir::Res::Def(hir::DefKind::TyAlias, alias_def_id)
                    | hir::Res::Def(hir::DefKind::AssocTy, alias_def_id) => {
                        let self_ty = self.tcx.type_of(alias_def_id).instantiate_identity();
                        let ty::TyKind::Adt(adt_def, _) = self_ty.kind() else { unreachable!() };
                        adt_def.variant_of_res(res.expect_non_local())
                    }
                    // Expect a struct, union, or enum variant, and get the corresponding ADT variant.
                    _ => self.tcx.expect_variant_res(res.expect_non_local()),
                };

                for field in &mut *fields {
                    let Some(field_def) = variant_def.fields.iter().find(|field_def| self.tcx.hygienic_eq(field.ident, field_def.ident(self.tcx), variant_def.def_id)) else {
                        span_bug!(field.span, "field `{}` does not match any field of {}", field.ident, self.tcx.def_path_str(variant_def.def_id));
                    };
                    // HACK: Copy ident syntax context from definition for correct sanitization later.
                    copy_def_span_ctxt(&mut field.ident, field_def.ident(self.tcx).span);
                    // NOTE: We have to disable shorthand syntax to ensure that
                    //       the correct field ident appears in printed code.
                    field.is_shorthand = false;
                }

                fields.flat_map_in_place(|field| self.flat_map_pat_field(field));

                return;
            }
            ast::PatKind::TupleStruct(qself, path, pats) => {
                let _res = self.sanitize_qualified_path(qself, path, pat_id);
                for pat in pats {
                    self.visit_pat(pat);
                }

                return;
            }
            _ => {}
        }

        if let Some(protected_ident) = protected_ident { self.protected_idents.insert(protected_ident); }
        ast::mut_visit::walk_pat(self, pat);
        if let Some(protected_ident) = protected_ident { self.protected_idents.remove(&protected_ident); }
    }

    fn visit_anon_const(&mut self, anon_const: &mut ast::AnonConst) {
        let Some(&def_id) = self.def_res.node_id_to_def_id.get(&anon_const.id) else { unreachable!() };

        // Store new context scope.
        let previous_scope = mem::replace(&mut self.current_scope, Some(def_id.to_def_id()));

        // Store new typeck scope.
        let previous_typeck_ctx = self.current_typeck_ctx;
        let typeck_root_def_id = self.tcx.typeck_root_def_id(def_id.to_def_id());
        let Some(typeck_root_local_def_id) = typeck_root_def_id.as_local() else { unreachable!() };
        if let Some(typeck_root_body_id) = self.tcx.hir_node_by_def_id(typeck_root_local_def_id).body_id() {
            self.current_typeck_ctx = Some(self.tcx.typeck_body(typeck_root_body_id));
        }

        ast::mut_visit::walk_anon_const(self, anon_const);

        // Restore previous context.
        self.current_typeck_ctx = previous_typeck_ctx;
        self.current_scope = previous_scope;
    }

    fn visit_path(&mut self, path: &mut ast::Path) {
        let Some(last_segment) = path.segments.last() else { unreachable!(); };
        let Some(res) = self.def_res.node_res(last_segment.id) else {
            span_bug!(path.span, "path `{}` cannot be resolved by generic path handler", ast::print::path_to_string(path));
        };
        let None = self.sanitize_path(path, res, None) else {
            span_bug!(path.span, "produced type-relative path in context which disallows qualified paths");
        };
    }

    fn visit_label(&mut self, label: &mut ast::Label) {
        sanitize_standalone_ident_if_from_expansion(&mut label.ident, IdentResKind::Label);
    }

    fn visit_ident(&mut self, ident: &mut Ident) {
        if self.protected_idents.contains(ident) { return; }
        sanitize_standalone_ident_if_from_expansion(ident, IdentResKind::Def);
    }

    fn visit_vis(&mut self, vis: &mut ast::Visibility) {
        match &mut vis.kind {
            ast::VisibilityKind::Restricted { path, id, .. } => {
                let Some(res) = self.def_res.node_res(*id) else {
                    span_bug!(vis.span, "restricted visibility path `{}` cannot be resolved", ast::print::path_to_string(path));
                };
                let None = self.adjust_path_from_expansion(path, res, None) else {
                    span_bug!(path.span, "produced type-relative path in context which disallows qualified paths");
                };
            }
            _ => ast::mut_visit::walk_vis(self, vis),
        }
    }
}

fn register_builtin_macros(syntax_extensions: &mut Vec<SyntaxExtension>) {
    fn dummy_syntax_extension(name: Symbol, helper_attrs: Vec<Symbol>, allow_internal_unstable: Option<Arc<[Symbol]>>) -> SyntaxExtension {
        SyntaxExtension {
            kind: SyntaxExtensionKind::NonMacroAttr,
            span: DUMMY_SP,
            allow_internal_unstable,
            stability: None,
            deprecation: None,
            helper_attrs,
            edition: Edition::Edition2024,
            builtin_name: Some(name),
            allow_internal_unsafe: false,
            local_inner_macros: false,
            collapse_debuginfo: true,
        }
    }

    syntax_extensions.extend([
        dummy_syntax_extension(sym::Default, vec![kw::Default], None), // #[derive(Default)]
    ]);
}

pub fn sanitize_macro_expansions<'tcx>(tcx: TyCtxt<'tcx>, crate_res: &res::CrateResolutions<'tcx>, def_res: &ast_lowering::DefResolutions, body_res: &ast_lowering::BodyResolutions<'tcx>, krate: &mut ast::Crate) {
    let mut syntax_extensions = vec![];
    register_builtin_macros(&mut syntax_extensions);

    let cstore = CStore::from_tcx(tcx);

    // HACK: The only way to enumerate the definitions of an external crate though the public API is
    //       by enumerating the def indices up to `CStore::num_def_ids_untracked`.
    //       See https://github.com/rust-lang/rust/pull/85889.
    let crate_def_indices = |cnum: hir::CrateNum| {
        const BASE_DEF_IDX: usize = hir::CRATE_DEF_INDEX.as_usize();
        let max_def_idx = cstore.num_def_ids_untracked(cnum);

        (BASE_DEF_IDX..max_def_idx).map(hir::DefIndex::from_usize)
    };

    // Find all loaded proc macro syntax extensions.
    tcx.crates(()).iter()
        // Ensure that we are only looking at crates with `CrateKind::ProcMacro`.
        .filter(|&&cnum| tcx.dep_kind(cnum).macros_only())
        .flat_map(|&cnum| crate_def_indices(cnum).map(move |index| hir::DefId { krate: cnum, index }))
        .filter(|&def_id| {
            // HACK: Not all def indices are encoded for proc macro crates, only the macro definitions themselves.
            //       To avoid crashes due to missing def index lookups, we must only generate "valid" indices,
            //       however, there is no stable mechanism for this.
            //       Instead, we use a def kind query to trigger a crate metadata lookup, and
            //       catch the panic, if it occurs.
            //       We also disable printing of panic messages with an empty hook.
            //       See https://github.com/rust-lang/rust/pull/76897.
            let builtin_panic_hook = panic::take_hook();
            panic::set_hook(Box::new(|_panic_info| {}));
            let Ok(def_kind) = panic::catch_unwind(panic::AssertUnwindSafe(|| cstore.def_kind_untracked(def_id))) else { return false; };
            panic::set_hook(builtin_panic_hook);

            matches!(def_kind, hir::DefKind::Macro(_))
        })
        .filter_map(|def_id| {
            match cstore.load_macro_untracked(def_id, tcx) {
                LoadedMacro::ProcMacro(syntax_extension) => Some(syntax_extension),
                // TODO: Generate syntax extensions from regular macros.
                LoadedMacro::MacroDef { def: _, ident: _, attrs: _, span: _, edition: _ } => None,
            }
        })
        .collect_into(&mut syntax_extensions);

    // Find the prelude module of this crate, whose contents are available in every module.
    let prelude_mod = tcx.hir_root_module().item_ids.iter().find_map(|&item_id| {
        let hir::ItemKind::Use(use_path, hir::UseKind::Glob) = tcx.hir_item(item_id).kind else { return None; };
        if !tcx.hir_attrs(item_id.hir_id()).iter().any(|attr| hir::attr::is_word_attr(attr, None, sym::prelude_import)) { return None; }
        let [.., last_segment] = use_path.segments else { unreachable!() };
        Some(last_segment.res.def_id())
    });

    let mut sanitizer = MacroExpansionSanitizer {
        tcx,
        crate_res,
        def_res,
        body_res,
        syntax_extensions,
        prelude_mod,
        current_scope: Some(LOCAL_CRATE.as_def_id()),
        current_typeck_ctx: None,
        macros_2_0_top_level_relative_path_res_hack: Macros2_0TopLevelRelativePathResHack::NotInMacros2_0Scope,
        protected_idents: Default::default(),
    };
    sanitizer.visit_crate(krate);

    let g = &tcx.sess.psess.attr_id_generator;

    // NOTE: Sanitization of paths may cause paths in struct expressions and patterns to become fully qualified paths,
    //       which are currently only supported with the `more_qualified_paths` feature.
    //       See https://github.com/rust-lang/rust/issues/86935.
    // #![feature(more_qualified_paths)]
    if !krate.attrs.iter().any(|attr| ast::inspect::is_list_attr_with_ident(attr, None, sym::feature, sym::more_qualified_paths)) {
        let feature_more_qualified_paths_attr = ast::mk::attr_inner(g, DUMMY_SP,
            Ident::new(sym::feature, DUMMY_SP),
            ast::mk::attr_args_delimited(DUMMY_SP, ast::token::Delimiter::Parenthesis, ast::mk::token_stream(vec![
                ast::mk::tt_token_joint(DUMMY_SP, ast::TokenKind::Ident(sym::more_qualified_paths, ast::token::IdentIsRaw::No)),
            ])),
        );
        krate.attrs.push(feature_more_qualified_paths_attr);
    }
}
