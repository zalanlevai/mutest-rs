use std::fmt::{self, Write};

use crate::ctxt::{TargetCtxt, TargetSpec, WorkspaceCtxt, workspace_target_display_str};

#[derive(Eq, PartialEq)]
pub enum Tab {
    Sources,
    Mutations,
    Tests,
}

pub(crate) fn base_html(body: &mut String, title: &str) -> Result<(), fmt::Error> {
    write!(body, "<!DOCTYPE html>")?;
    write!(body, "<html lang=\"en\">")?;
    write!(body, "<head>")?;
    write!(body, "<meta charset=\"UTF-8\">")?;
    write!(body, "<title>{}</title>", title)?;
    write!(body, "<link rel=\"stylesheet\" href=\"/static/styles.css\">")?;
    write!(body, "</head>")?;
    Ok(())
}

pub(crate) fn topbar_html(body: &mut String, wcx: &WorkspaceCtxt, package: &str, target: &TargetSpec, tcx: &TargetCtxt, active_tab: Option<Tab>) -> Result<(), fmt::Error> {
    let target_path = format!("{}/{}", package, target.path_str());

    let mutations_count = tcx.mutations_count();
    let tests_count = tcx.tests_count();

    write!(body, "<nav class=\"topbar\">")?;
    write!(body, "<a class=\"logo\" href=\"/\">mutest-rs</a>")?;

    write!(body, "<button popovertarget=\"target-selector\">{}</button>", workspace_target_display_str(package, target))?;
    write!(body, "<nav id=\"target-selector\" popover=\"auto\">")?;
    for (option_package, option_target) in wcx.targets() {
        write!(body, "<a href=\"/{}/{}\" class=\"item\">{}</a>", option_package, option_target.path_str(), workspace_target_display_str(option_package, option_target))?;
    }
    write!(body, "</nav>")?;

    write!(body, "<a class=\"tab")?;
    if active_tab == Some(Tab::Sources) { write!(body, " active")?; }
    write!(body, "\" href=\"/{}/source\">sources</a>", target_path)?;

    write!(body, "<a class=\"tab")?;
    if active_tab == Some(Tab::Mutations) { write!(body, " active")?; }
    write!(body, "\" href=\"/{}/mutations\">mutations <span class=\"badge\">{}</span></a>", target_path, mutations_count)?;

    write!(body, "<a class=\"tab")?;
    if active_tab == Some(Tab::Tests) { write!(body, " active")?; }
    write!(body, "\" href=\"/{}/tests\">tests <span class=\"badge\">{}</span></a>", target_path, tests_count)?;

    write!(body, "</nav>")?;
    Ok(())
}
