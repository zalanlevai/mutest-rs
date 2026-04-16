use std::fmt::{self, Write};

use crate::ctxt::WebCtxt;

#[derive(Eq, PartialEq)]
pub enum Tab {
    Sources,
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

pub(crate) fn topbar_html(body: &mut String, _wcx: &WebCtxt, active_tab: Option<Tab>) -> Result<(), fmt::Error> {
    write!(body, "<nav class=\"topbar\">")?;
    write!(body, "<a class=\"logo\" href=\"/\">mutest-rs</a>")?;

    write!(body, "<a class=\"tab")?;
    if active_tab == Some(Tab::Sources) { write!(body, " active")?; }
    write!(body, "\" href=\"/source\">sources</a>")?;

    write!(body, "</nav>")?;
    Ok(())
}
