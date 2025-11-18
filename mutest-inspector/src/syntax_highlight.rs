use std::fmt::Write;

use pulldown_cmark_escape::escape_html_body_text;

macro_rules! highlight_names {
    ($($name:literal => $shortcode:literal,)+) => {
        const HIGHLIGHT_NAMES: &[&str] = &[$($name,)+];
        const HIGHLIGHT_SHORTCODES: &[&str] = &[$($shortcode,)+];
    }
}

highlight_names! {
    "attribute" => "attr",
    "comment" => "comment",
    "constant" => "const",
    "constant.builtin" => "const!",
    "function" => "fn",
    "function.builtin" => "fn!",
    "keyword" => "kw",
    "label" => "label",
    "macro" => "macro",
    "module" => "mod",
    "number" => "num",
    "operator" => "op",
    "property" => "prop",
    "property.builtin" => "prop!",
    "punctuation" => "p",
    "punctuation.bracket" => "pb",
    "punctuation.delimiter" => "pd",
    "punctuation.special" => "p!",
    "string" => "str",
    "string.escape" => "escape",
    "string.special" => "str!",
    "tag" => "tag",
    "type" => "ty",
    "type.builtin" => "ty!",
    "variable" => "var",
    "variable.builtin" => "var!",
    "variable.parameter" => "param",
}

pub struct SyntaxHighlighter {
    highlighter: tree_sitter_highlight::Highlighter,
    highlight_config: tree_sitter_highlight::HighlightConfiguration,
}

impl SyntaxHighlighter {
    pub fn init() -> Self {
        let highlighter = tree_sitter_highlight::Highlighter::new();
        let mut highlight_config = tree_sitter_highlight::HighlightConfiguration::new(
            tree_sitter_rust::LANGUAGE.into(),
            "rust",
            tree_sitter_rust::HIGHLIGHTS_QUERY,
            tree_sitter_rust::INJECTIONS_QUERY,
            "",
        ).expect("cannot load tree-sitter language definition");
        highlight_config.configure(HIGHLIGHT_NAMES);

        SyntaxHighlighter { highlighter, highlight_config }
    }

    pub fn highlight_lines_html(&mut self, source: &str) -> Result<Vec<String>, tree_sitter_highlight::Error> {
        let highlight_events = self.highlighter.highlight(&self.highlight_config, source.as_bytes(), None, |_| None)?;

        let mut highlighted_lines_html = vec![String::new()];

        for highlight_event in highlight_events {
            let highlight_event = highlight_event?;

            match highlight_event {
                tree_sitter_highlight::HighlightEvent::Source { start, end } => {
                    let source_snippet = &source[start..end];

                    let mut source_snippet_lines = source_snippet.lines();
                    if let Some(source_snippet) = source_snippet_lines.next() {
                        escape_html_body_text(highlighted_lines_html.last_mut().unwrap(), source_snippet).unwrap();
                    }
                    for source_snippet in source_snippet_lines {
                        let mut sanitized_source_snippet = String::new();
                        escape_html_body_text(&mut sanitized_source_snippet, source_snippet).unwrap();
                        highlighted_lines_html.push(sanitized_source_snippet);
                    }

                    // NOTE: The `str::lines()` iterator ignores trailing newlines, which we want to keep.
                    if source_snippet.ends_with("\n") || source_snippet.ends_with("\r\n") {
                        highlighted_lines_html.push(String::new());
                    }
                }
                tree_sitter_highlight::HighlightEvent::HighlightStart(highlight) => {
                    write!(highlighted_lines_html.last_mut().unwrap(), "<span class=\"{}\">", HIGHLIGHT_SHORTCODES[highlight.0]).unwrap();
                }
                tree_sitter_highlight::HighlightEvent::HighlightEnd => {
                    write!(highlighted_lines_html.last_mut().unwrap(), "</span>").unwrap();
                }
            }
        }

        Ok(highlighted_lines_html)
    }
}
