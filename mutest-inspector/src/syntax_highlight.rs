use std::fmt::Write;

use pulldown_cmark_escape::escape_html_body_text;

#[derive(Clone, Debug)]
pub struct MappedLineLoc {
    pub source_line_byte_offset: usize,
    pub mapped_line_byte_offset: usize,
}

#[derive(Clone, Debug)]
pub struct MappedLineSegment {
    pub start_loc: MappedLineLoc,
    pub end_loc: MappedLineLoc,
    pub breakable: bool,
}

#[derive(Clone, Debug)]
pub struct MappedLine {
    line: String,
    mappings: Vec<MappedLineSegment>,
}

impl MappedLine {
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.line
    }

    #[inline]
    pub fn mappings(&self) -> &[MappedLineSegment] {
        &self.mappings
    }

    pub fn insert_unbreakable_segment(&mut self, start_source_offset: usize, end_source_offset: usize, prefix: &str, suffix: &str) {
        let Some(start_segment_idx) = self.mappings.iter().position(|segment| segment.end_loc.source_line_byte_offset > start_source_offset) else {
            panic!("invalid span overlay: cannot find source segment starting at or before span overlay start");
        };
        let start_segment = self.mappings[start_segment_idx].clone();

        let Some(end_segment_idx) = self.mappings.iter().position(|segment| segment.end_loc.source_line_byte_offset >= end_source_offset) else {
            panic!("invalid span overlay: cannot find source segment starting at or after span overlay end");
        };
        let end_segment = self.mappings[end_segment_idx].clone();

        let start_source_line_adjusted_byte_offset = match start_segment.breakable {
            true => start_source_offset,
            false => start_segment.start_loc.source_line_byte_offset,
        };
        let start_mapped_line_byte_offset = match start_segment.breakable {
            true => start_segment.start_loc.mapped_line_byte_offset + (start_source_offset - start_segment.start_loc.source_line_byte_offset),
            false => start_segment.start_loc.mapped_line_byte_offset,
        };

        let end_source_line_adjusted_byte_offset = match end_segment.breakable {
            true => end_source_offset,
            false => end_segment.end_loc.source_line_byte_offset,
        };
        let end_mapped_line_byte_offset = match end_segment.breakable {
            true => end_segment.start_loc.mapped_line_byte_offset + (end_source_offset - end_segment.start_loc.source_line_byte_offset),
            false => end_segment.end_loc.mapped_line_byte_offset,
        };

        self.line.insert_str(end_mapped_line_byte_offset, suffix);
        self.line.insert_str(start_mapped_line_byte_offset, prefix);
        let insert_offset = prefix.len() + suffix.len();

        let replacement_segment_start_loc = MappedLineLoc {
            source_line_byte_offset: start_source_line_adjusted_byte_offset,
            mapped_line_byte_offset: start_mapped_line_byte_offset,
        };
        let replacement_segment_end_loc = MappedLineLoc {
            source_line_byte_offset: end_source_line_adjusted_byte_offset,
            mapped_line_byte_offset: end_mapped_line_byte_offset + insert_offset,
        };

        self.mappings.splice(start_segment_idx..=end_segment_idx, [
            MappedLineSegment {
                start_loc: replacement_segment_start_loc.clone(),
                end_loc: replacement_segment_end_loc.clone(),
                breakable: false,
            },
        ]);
        // NOTE: The previous splice invalidates the original end segment index.
        let mut tail_segments_start_idx = start_segment_idx + 1;

        // Add the rest of the broken apart end segment as a breakable suffix segment.
        if end_source_line_adjusted_byte_offset != end_segment.end_loc.source_line_byte_offset {
            tail_segments_start_idx += 1;

            // Adjust suffix segment's mapped end offset with insertion offset.
            let mut suffix_segment_end_loc = end_segment.end_loc;
            suffix_segment_end_loc.mapped_line_byte_offset += insert_offset;

            self.mappings.insert(start_segment_idx + 1, MappedLineSegment {
                start_loc: replacement_segment_end_loc,
                end_loc: suffix_segment_end_loc,
                breakable: true,
            });
        }
        // Add the rest of the broken apart start segment as a breakable prefix segment.
        if start_source_line_adjusted_byte_offset != start_segment.start_loc.source_line_byte_offset {
            tail_segments_start_idx += 1;
            self.mappings.insert(start_segment_idx, MappedLineSegment {
                start_loc: start_segment.start_loc,
                end_loc: replacement_segment_start_loc,
                breakable: true,
            });
        }

        // Adjust tail segments' mapped start and end offsets with insertion offsets.
        for segment in &mut self.mappings[tail_segments_start_idx..] {
            segment.start_loc.mapped_line_byte_offset += insert_offset;
            segment.end_loc.mapped_line_byte_offset += insert_offset;
        }
    }
}

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

    pub fn highlight_lines_html(&mut self, source: &str) -> Result<Vec<MappedLine>, tree_sitter_highlight::Error> {
        let highlight_events = self.highlighter.highlight(&self.highlight_config, source.as_bytes(), None, |_| None)?;

        let mut highlighted_lines_html = vec![MappedLine { line: String::new(), mappings: vec![] }];

        let mut active_highlight: Option<tree_sitter_highlight::Highlight> = None;
        let mut current_line_source_offset = 0;

        for highlight_event in highlight_events {
            let highlight_event = highlight_event?;

            match highlight_event {
                tree_sitter_highlight::HighlightEvent::Source { start, end } => {
                    // NOTE: Based on `pulldown_cmark_escape::escape_html_body_text`.
                    fn escape_html_in_source_line_snippet_with_unbreakable_segments(out: &mut MappedLine, source_line_snippet: &str, snippet_start_loc: MappedLineLoc) {
                        let mut last_unescaped_segment_start_loc = snippet_start_loc.clone();

                        for (source_snippet_byte_offset, &source_snippet_byte) in source_line_snippet.as_bytes().iter().enumerate() {
                            let html_escape_str = match source_snippet_byte {
                                b'&' => "&amp;",
                                b'<' => "&lt;",
                                b'>' => "&gt;",
                                _ => { continue; }
                            };

                            let last_unescaped_segment_start_source_snippet_byte_offset = last_unescaped_segment_start_loc.source_line_byte_offset - snippet_start_loc.source_line_byte_offset;
                            out.line.push_str(&source_line_snippet[last_unescaped_segment_start_source_snippet_byte_offset..source_snippet_byte_offset]);

                            let escape_start_loc = MappedLineLoc {
                                source_line_byte_offset: snippet_start_loc.source_line_byte_offset + source_snippet_byte_offset,
                                mapped_line_byte_offset: out.line.len(),
                            };

                            if escape_start_loc.source_line_byte_offset > last_unescaped_segment_start_loc.source_line_byte_offset {
                                out.mappings.push(MappedLineSegment {
                                    start_loc: last_unescaped_segment_start_loc,
                                    end_loc: escape_start_loc.clone(),
                                    breakable: true,
                                });
                            }

                            out.line.push_str(html_escape_str);

                            let escape_end_loc = MappedLineLoc {
                                source_line_byte_offset: escape_start_loc.source_line_byte_offset + 1,
                                mapped_line_byte_offset: out.line.len(),
                            };

                            out.mappings.push(MappedLineSegment {
                                start_loc: escape_start_loc,
                                end_loc: escape_end_loc.clone(),
                                breakable: false,
                            });

                            last_unescaped_segment_start_loc = escape_end_loc;
                        }

                        let last_unescaped_segment_start_source_snippet_byte_offset = last_unescaped_segment_start_loc.source_line_byte_offset - snippet_start_loc.source_line_byte_offset;
                        out.line.push_str(&source_line_snippet[last_unescaped_segment_start_source_snippet_byte_offset..]);

                        let snippet_end_loc = MappedLineLoc {
                            source_line_byte_offset: snippet_start_loc.source_line_byte_offset + source_line_snippet.len(),
                            mapped_line_byte_offset: out.line.len(),
                        };

                        if snippet_end_loc.source_line_byte_offset > last_unescaped_segment_start_loc.source_line_byte_offset {
                            out.mappings.push(MappedLineSegment {
                                start_loc: last_unescaped_segment_start_loc,
                                end_loc: snippet_end_loc,
                                breakable: true,
                            });
                        }
                    }

                    let source_snippet = &source[start..end];

                    let mut source_snippet_lines = source_snippet.lines();
                    if let Some(source_snippet) = source_snippet_lines.next() && !source_snippet.is_empty() {
                        let current_highlighted_line = highlighted_lines_html.last_mut().unwrap();

                        // NOTE: The explicit remapping of HTML escape codes through unbreakable segments
                        //       only needs to be done if we are not in a highlight,
                        //       because we already make unbreakable segments for highlights:
                        //       one for the entire source snippet (including the prefix and suffix).
                        match active_highlight {
                            None => {
                                let start_loc = MappedLineLoc {
                                    source_line_byte_offset: current_line_source_offset,
                                    mapped_line_byte_offset: current_highlighted_line.line.len(),
                                };
                                escape_html_in_source_line_snippet_with_unbreakable_segments(current_highlighted_line, source_snippet, start_loc);
                                current_line_source_offset += source_snippet.len();
                            }
                            Some(highlight) => {
                                let line_highlight_start = MappedLineLoc {
                                    source_line_byte_offset: current_line_source_offset,
                                    mapped_line_byte_offset: current_highlighted_line.line.len(),
                                };

                                write!(current_highlighted_line.line, "<span class=\"{}\">", HIGHLIGHT_SHORTCODES[highlight.0]).unwrap();

                                escape_html_body_text(&mut current_highlighted_line.line, source_snippet).unwrap();
                                current_line_source_offset += source_snippet.len();

                                write!(current_highlighted_line.line, "</span>").unwrap();

                                let line_highlight_end = MappedLineLoc {
                                    source_line_byte_offset: current_line_source_offset,
                                    mapped_line_byte_offset: current_highlighted_line.line.len(),
                                };

                                current_highlighted_line.mappings.push(MappedLineSegment {
                                    start_loc: line_highlight_start,
                                    end_loc: line_highlight_end,
                                    breakable: false,
                                });
                            }
                        }
                    }
                    for source_snippet in source_snippet_lines {
                        current_line_source_offset = 0;

                        let mut new_highlighted_line = MappedLine { line: String::new(), mappings: vec![] };

                        // NOTE: The explicit remapping of HTML escape codes through unbreakable segments
                        //       only needs to be done if we are not in a highlight,
                        //       because we already make unbreakable segments for highlights:
                        //       one for the entire source snippet (including the prefix and suffix).
                        match active_highlight {
                            None => {
                                let start_loc = MappedLineLoc {
                                    source_line_byte_offset: 0,
                                    mapped_line_byte_offset: 0,
                                };
                                escape_html_in_source_line_snippet_with_unbreakable_segments(&mut new_highlighted_line, source_snippet, start_loc);
                                current_line_source_offset += source_snippet.len();
                            }
                            Some(highlight) => {
                                let line_highlight_start = MappedLineLoc {
                                    source_line_byte_offset: 0,
                                    mapped_line_byte_offset: 0,
                                };

                                write!(new_highlighted_line.line, "<span class=\"{}\">", HIGHLIGHT_SHORTCODES[highlight.0]).unwrap();

                                escape_html_body_text(&mut new_highlighted_line.line, source_snippet).unwrap();
                                current_line_source_offset += source_snippet.len();

                                write!(new_highlighted_line.line, "</span>").unwrap();

                                let line_highlight_end = MappedLineLoc {
                                    source_line_byte_offset: current_line_source_offset,
                                    mapped_line_byte_offset: new_highlighted_line.line.len(),
                                };

                                new_highlighted_line.mappings.push(MappedLineSegment {
                                    start_loc: line_highlight_start,
                                    end_loc: line_highlight_end,
                                    breakable: false,
                                });
                            }
                        }

                        highlighted_lines_html.push(new_highlighted_line);
                    }

                    // NOTE: The `str::lines()` iterator ignores trailing newlines, which we want to keep.
                    if source_snippet.ends_with("\n") || source_snippet.ends_with("\r\n") {
                        highlighted_lines_html.push(MappedLine { line: String::new(), mappings: vec![] });
                        current_line_source_offset = 0;
                    }
                }
                tree_sitter_highlight::HighlightEvent::HighlightStart(highlight) => {
                    active_highlight = Some(highlight);
                }
                tree_sitter_highlight::HighlightEvent::HighlightEnd => {
                    active_highlight = None;
                }
            }
        }

        Ok(highlighted_lines_html)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl MappedLine {
        fn source_segments_debug_repr(&self, source: &str) -> String {
            let mut out = source.to_owned();
            for segment in self.mappings.iter().rev() {
                let (start_token, end_token) = match segment.breakable {
                    true => ("<:", ":>"),
                    false => ("<|", "|>"),
                };
                out.insert_str(segment.end_loc.source_line_byte_offset, end_token);
                out.insert_str(segment.start_loc.source_line_byte_offset, start_token);
            }
            out
        }

        fn mapped_segments_debug_repr(&self) -> String {
            let mut out = self.line.to_owned();
            for segment in self.mappings.iter().rev() {
                let (start_token, end_token) = match segment.breakable {
                    true => ("<:", ":>"),
                    false => ("<|", "|>"),
                };
                out.insert_str(segment.end_loc.mapped_line_byte_offset, end_token);
                out.insert_str(segment.start_loc.mapped_line_byte_offset, start_token);
            }
            out
        }
    }

    macro_rules! check_line_segments {
        (
            $mapped_line:expr, $source_line:expr,
            source: $source:literal
            mapped: $mapped:literal
        ) => {
            assert_eq!($source, $mapped_line.source_segments_debug_repr($source_line), "source line segments mismatch");
            assert_eq!($mapped, $mapped_line.mapped_segments_debug_repr(), "mapped line segments mismatch");
        };
    }

    macro_rules! check_segmented_lines {
        (
            $mapped_lines:expr, $source:expr,
            $(
                source: $source_line:literal
                mapped: $mapped_line:literal
            )+
        ) => {
            let mut line_no = 1;
            let mut mapped_lines = $mapped_lines.iter();
            let mut source_lines = $source.lines();

            $(
                let Some(mapped_line) = mapped_lines.next() else { panic!("too few mapped lines: expected line {line_no}"); };
                let Some(source_line) = source_lines.next() else { panic!("too few source lines: expected line {line_no}"); };
                assert_eq!($source_line, mapped_line.source_segments_debug_repr(source_line), "line {line_no}: source line segments mismatch");
                assert_eq!($mapped_line, mapped_line.mapped_segments_debug_repr(), "line {line_no}: mapped line segments mismatch");
                line_no += 1;
            )+

            if let Some(mapped_line) = mapped_lines.next() {
                panic!("too few lines checked: found line {line_no} {:?}", mapped_line.mapped_segments_debug_repr());
            }
        };
    }

    #[test]
    fn test_highlight_lines_keeps_empty_last_line() {
        let source = concat!(
            "fn foo() {}\n",
            "",
        );

        let mut syntax_highlighter = SyntaxHighlighter::init();
        let mapped_lines = syntax_highlighter.highlight_lines_html(source).unwrap();

        assert_eq!("", mapped_lines[1].mapped_segments_debug_repr());
    }

    #[test]
    fn test_highlight_lines_ensures_no_empty_line_delimiter_segments() {
        let source = concat!(
            "    let mut a = 0;\n",
            "    let mut b = a;",
        );

        let mut syntax_highlighter = SyntaxHighlighter::init();
        let mapped_lines = syntax_highlighter.highlight_lines_html(source).unwrap();

        check_segmented_lines!(mapped_lines, source,
            source: "<:    :><|let|><: :><|mut|><: a = :><|0|><|;|>"
            mapped: "<:    :><|<span class=\"kw\">let</span>|><: :><|<span class=\"kw\">mut</span>|><: a = :><|<span class=\"const!\">0</span>|><|<span class=\"pd\">;</span>|>"

            source: "<:    :><|let|><: :><|mut|><: b = a:><|;|>"
            mapped: "<:    :><|<span class=\"kw\">let</span>|><: :><|<span class=\"kw\">mut</span>|><: b = a:><|<span class=\"pd\">;</span>|>"
        );
    }

    #[test]
    fn test_multiline_highlights() {
        let source = concat!(
            "    println!(\"\\\n",
            "        testing\\\n",
            "        123\\\n",
            "\n", // Empty line in the middle of a multiline highlight.
            "    \");",
        );

        let mut syntax_highlighter = SyntaxHighlighter::init();
        let mapped_lines = syntax_highlighter.highlight_lines_html(source).unwrap();

        check_segmented_lines!(mapped_lines, source,
            source: "<:    :><|println|><|!|><|(|><|\"\\|>"
            mapped: "<:    :><|<span class=\"fn\">println</span>|><|<span class=\"fn\">!</span>|><|<span class=\"pb\">(</span>|><|<span class=\"str\">\"\\</span>|>"

            source: "<|        testing\\|>"
            mapped: "<|<span class=\"str\">        testing\\</span>|>"

            source: "<|        123\\|>"
            mapped: "<|<span class=\"str\">        123\\</span>|>"

            // Empty line in the middle of a multiline highlight.
            source: "<||>"
            mapped: "<|<span class=\"str\"></span>|>"

            source: "<|    \"|><|)|><|;|>"
            mapped: "<|<span class=\"str\">    \"</span>|><|<span class=\"pb\">)</span>|><|<span class=\"pd\">;</span>|>"
        );
    }

    #[test]
    fn test_highlight_lines_mappings_record_html_escapes_as_unbreakable_segments() {
        let source = "    Ok(_) => i += 1,";

        let mut syntax_highlighter = SyntaxHighlighter::init();
        let mapped_lines = syntax_highlighter.highlight_lines_html(source).unwrap();

        check_segmented_lines!(mapped_lines, source,
            source: "<:    :><|Ok|><|(|><:_:><|)|><: =:><|>|><: i += :><|1|><|,|>"
            mapped: "<:    :><|<span class=\"fn\">Ok</span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><: =:><|&gt;|><: i += :><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );
    }

    #[test]
    fn test_highlight_lines_ensures_no_empty_segments_around_html_escapes() {
        let source = "<T as Default>::default()";

        let mut syntax_highlighter = SyntaxHighlighter::init();
        let mapped_lines = syntax_highlighter.highlight_lines_html(source).unwrap();

        check_segmented_lines!(mapped_lines, source,
            source: "<|<|><|T|><: :><|as|><: :><|Default|><|>|><|::|><|default|><|(|><|)|>"
            mapped: "<|&lt;|><|<span class=\"ty\">T</span>|><: :><|<span class=\"kw\">as</span>|><: :><|<span class=\"ty\">Default</span>|><|&gt;|><|<span class=\"pd\">::</span>|><|<span class=\"fn\">default</span>|><|<span class=\"pb\">(</span>|><|<span class=\"pb\">)</span>|>"
        );
    }

    #[test]
    fn test_mapped_line_insert_unbreakable_segment() {
        // TODO: Change source to something different / more complex.
        let source = "    Ok(_) => i += 1,";

        let mut syntax_highlighter = SyntaxHighlighter::init();
        let mapped_lines = syntax_highlighter.highlight_lines_html(source).unwrap();
        let [mapped_line] = &mapped_lines[..] else { unreachable!() };

        check_line_segments!(mapped_line, source,
            source: "<:    :><|Ok|><|(|><:_:><|)|><: =:><|>|><: i += :><|1|><|,|>"
            mapped: "<:    :><|<span class=\"fn\">Ok</span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><: =:><|&gt;|><: i += :><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );

        // TEST: Start at existing segment boundary / end at existing segment boundary.
        let mut new_mapped_line = mapped_line.clone();
        new_mapped_line.insert_unbreakable_segment(4, 9, "<span class=\"test-segment\">", "</span>");
        check_line_segments!(new_mapped_line, source,
            source: "<:    :><|Ok(_)|><: =:><|>|><: i += :><|1|><|,|>"
            mapped: "<:    :><|<span class=\"test-segment\"><span class=\"fn\">Ok</span><span class=\"pb\">(</span>_<span class=\"pb\">)</span></span>|><: =:><|&gt;|><: i += :><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );

        // TEST: Start inside unbreakable segment / end inside the same unbreakable segment.
        let mut new_mapped_line = mapped_line.clone();
        new_mapped_line.insert_unbreakable_segment(5, 6, "<span class=\"test-segment\">", "</span>");
        check_line_segments!(new_mapped_line, source,
            source: "<:    :><|Ok|><|(|><:_:><|)|><: =:><|>|><: i += :><|1|><|,|>"
            mapped: "<:    :><|<span class=\"test-segment\"><span class=\"fn\">Ok</span></span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><: =:><|&gt;|><: i += :><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );

        // TEST: Start inside breakable segment / end inside the same breakable segment.
        let mut new_mapped_line = mapped_line.clone();
        new_mapped_line.insert_unbreakable_segment(1, 2, "<span class=\"test-segment\">", "</span>");
        check_line_segments!(new_mapped_line, source,
            source: "<: :><| |><:  :><|Ok|><|(|><:_:><|)|><: =:><|>|><: i += :><|1|><|,|>"
            mapped: "<: :><|<span class=\"test-segment\"> </span>|><:  :><|<span class=\"fn\">Ok</span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><: =:><|&gt;|><: i += :><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );

        // TEST: Start inside breakable segment / end inside another breakable segment.
        let mut new_mapped_line = mapped_line.clone();
        new_mapped_line.insert_unbreakable_segment(10, 17, "<span class=\"test-segment\">", "</span>");
        check_line_segments!(new_mapped_line, source,
            source: "<:    :><|Ok|><|(|><:_:><|)|><: :><|=> i +=|><: :><|1|><|,|>"
            mapped: "<:    :><|<span class=\"fn\">Ok</span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><: :><|<span class=\"test-segment\">=&gt; i +=</span>|><: :><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );

        // TEST: Start at breakable segment boundary / end at another breakable segment boundary.
        let mut new_mapped_line = mapped_line.clone();
        new_mapped_line.insert_unbreakable_segment(9, 18, "<span class=\"test-segment\">", "</span>");
        check_line_segments!(new_mapped_line, source,
            source: "<:    :><|Ok|><|(|><:_:><|)|><| => i += |><|1|><|,|>"
            mapped: "<:    :><|<span class=\"fn\">Ok</span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><|<span class=\"test-segment\"> =&gt; i += </span>|><|<span class=\"const!\">1</span>|><|<span class=\"pd\">,</span>|>"
        );

        // TEST: Start at last segment boundary / end at last segment boundary.
        let mut new_mapped_line = mapped_line.clone();
        new_mapped_line.insert_unbreakable_segment(19, 20, "<span class=\"test-segment\">", "</span>");
        check_line_segments!(new_mapped_line, source,
            source: "<:    :><|Ok|><|(|><:_:><|)|><: =:><|>|><: i += :><|1|><|,|>"
            mapped: "<:    :><|<span class=\"fn\">Ok</span>|><|<span class=\"pb\">(</span>|><:_:><|<span class=\"pb\">)</span>|><: =:><|&gt;|><: i += :><|<span class=\"const!\">1</span>|><|<span class=\"test-segment\"><span class=\"pd\">,</span></span>|>"
        );
    }
}
