use std::fmt::Write;

use mutest_json::data_structures::IdxVec;
use pulldown_cmark_escape::escape_html_body_text;

use crate::source_file::LineNo;
use crate::syntax_highlight::MappedLine;

pub mod mutations;

#[derive(Debug)]
pub struct SourceFileHtml {
    pub display_path_html: String,
    pub highlighted_lines_html: IdxVec<LineNo, MappedLine>,
}

pub fn escape_html_body_text_with_inline_code(text: &str) -> String {
    let mut s = String::new();

    let parts = text.split('`');
    for (i, part) in parts.enumerate() {
        if part.is_empty() { continue; }
        let code_part = (i % 2) == 1;

        if code_part { write!(s, "<span class=\"inline-code\">").unwrap(); }
        escape_html_body_text(&mut s, part).unwrap();
        if code_part { write!(s, "</span>").unwrap(); }
    }

    s
}

pub fn source_code_line_content(line_html: &str) -> &str {
    match line_html.is_empty() {
        false => line_html,
        // NOTE: This is used to ensure both that empty lines render correctly, and
        //       that copying them in browsers results in a newline.
        true => "\n",
    }
}
