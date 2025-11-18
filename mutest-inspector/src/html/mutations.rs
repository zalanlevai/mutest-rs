use std::collections::HashMap;
use std::path::PathBuf;

use mutest_json::mutations::MutationId;

use crate::ctxt::WebCtxt;
use crate::source_file::LineNo;
use crate::syntax_highlight::SyntaxHighlighter;

#[derive(Debug)]
pub struct SubstHtml {
    pub start_line: LineNo,
    pub original_lines_html: Vec<String>,
    pub replacement_lines_html: Vec<String>,
}

impl SubstHtml {
    pub fn end_line(&self) -> LineNo {
        LineNo(self.start_line.0 + self.original_lines_html.len() as u32 - (!self.original_lines_html.is_empty() as u32))
    }
}

#[derive(Debug)]
pub struct MutationHtml {
    pub display_name_html: String,
    pub subst_htmls: Vec<SubstHtml>,
}

pub fn render_mutation_subst_lines(wcx: &WebCtxt, syntax_highlighter: &mut SyntaxHighlighter, subst: &mutest_json::mutations::Substitution) -> Option<SubstHtml> {
    let subst_span = subst.location.span();
    let subst_start_line = LineNo(subst_span.begin.0 as u32);
    let subst_end_line = LineNo(subst_span.end.0 as u32);

    let Some(source_file) = wcx.loaded_source_file(&subst_span.path) else { return None; };
    let Some(source_file_html) = wcx.source_file_html(&subst_span.path) else { return None; };

    let start_line_start_byte_offset = {
        let mut char_indices_iter = source_file.lines[subst_start_line].char_indices();
        for _ in 1..subst_span.begin.1 { char_indices_iter.next(); }
        char_indices_iter.offset()
    };
    let prefix_original_source = &source_file.lines[subst_start_line][..start_line_start_byte_offset];

    let end_line_end_byte_offset = {
        let mut char_indices_iter = source_file.lines[subst_end_line].char_indices();
        for _ in 1..subst_span.end.1 { char_indices_iter.next(); }
        char_indices_iter.offset()
    };
    let suffix_original_source = &source_file.lines[subst_end_line][end_line_end_byte_offset..];

    let start_line_whitespace_prefix_byte_offset = 'offset: {
        let mut char_indices_iter = source_file.lines[subst_start_line].char_indices();
        while let Some((offset, c)) = char_indices_iter.next() {
            if !c.is_whitespace() { break 'offset offset; }
        }
        0
    };
    let original_source_start_line_whitespace_prefix = &source_file.lines[subst_start_line][..start_line_whitespace_prefix_byte_offset];

    let (original_lines_html, replacement_text) = match subst.location {
        mutest_json::mutations::SubstitutionLocation::Replace(_) => {
            let mut replacement_text = String::new();
            replacement_text.push_str(prefix_original_source);

            let mut replacement_lines_iter = subst.substitute.replacement.lines();
            if let Some(replacement_line) = replacement_lines_iter.next() {
                replacement_text.push_str(replacement_line);
            }
            // Indent subsequent lines with the whitespace indentation of the original source line.
            for replacement_line in replacement_lines_iter {
                replacement_text.push_str("\n");
                replacement_text.push_str(original_source_start_line_whitespace_prefix);
                replacement_text.push_str(replacement_line);
            }

            replacement_text.push_str(suffix_original_source);

            let original_lines_html = source_file_html.highlighted_lines_html[subst_start_line..=subst_end_line].to_vec();
            (original_lines_html, replacement_text)
        }
        mutest_json::mutations::SubstitutionLocation::InsertBefore(_) => {
            let is_inline_insertion = !prefix_original_source.chars().all(|c| c.is_whitespace());
            match is_inline_insertion {
                true => {
                    let mut replacement_text = String::new();
                    replacement_text.push_str(prefix_original_source);
                    replacement_text.push_str(&subst.substitute.replacement);
                    replacement_text.push_str(" ");
                    replacement_text.push_str(&source_file.lines[subst_start_line][start_line_start_byte_offset..]);

                    let original_lines_html = vec![source_file_html.highlighted_lines_html[subst_start_line].clone()];
                    (original_lines_html, replacement_text)
                }
                false => {
                    let mut replacement_text = String::new();

                    replacement_text.push_str(prefix_original_source);

                    let mut replacement_lines_iter = subst.substitute.replacement.lines();
                    if let Some(replacement_line) = replacement_lines_iter.next() {
                        replacement_text.push_str(replacement_line);
                    }
                    // Indent subsequent lines with the whitespace indentation of the original source line.
                    for replacement_line in replacement_lines_iter {
                        replacement_text.push_str("\n");
                        replacement_text.push_str(original_source_start_line_whitespace_prefix);
                        replacement_text.push_str(replacement_line);
                    }

                    (vec![], replacement_text)
                }
            }
        }
        mutest_json::mutations::SubstitutionLocation::InsertAfter(_) => {
            let is_inline_insertion = !suffix_original_source.chars().all(|c| c.is_whitespace());
            match is_inline_insertion {
                true => {
                    let mut replacement_text = String::new();
                    replacement_text.push_str(&source_file.lines[subst_end_line][..end_line_end_byte_offset]);
                    replacement_text.push_str(" ");
                    replacement_text.push_str(&subst.substitute.replacement);
                    replacement_text.push_str(suffix_original_source);

                    let original_lines_html = vec![source_file_html.highlighted_lines_html[subst_end_line].clone()];
                    (original_lines_html, replacement_text)
                }
                false => {
                    let mut replacement_text = String::new();

                    let mut replacement_lines_iter = subst.substitute.replacement.lines();
                    if let Some(replacement_line) = replacement_lines_iter.next() {
                        replacement_text.push_str(replacement_line);
                    }
                    // Indent subsequent lines with the whitespace indentation of the original source line.
                    for replacement_line in replacement_lines_iter {
                        replacement_text.push_str("\n");
                        replacement_text.push_str(original_source_start_line_whitespace_prefix);
                        replacement_text.push_str(replacement_line);
                    }

                    replacement_text.push_str(suffix_original_source);

                    (vec![], replacement_text)
                }
            }
        }
    };

    let highlighted_lines_html = syntax_highlighter.highlight_lines_html(&replacement_text)
        .expect(&format!("cannot syntax highlight mutation substitution `{}`", subst.substitute.replacement));

    Some(SubstHtml {
        start_line: subst_start_line,
        original_lines_html,
        replacement_lines_html: highlighted_lines_html,
    })
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum OverlappingGroupKind {
    InsertBefore,
    Replace,
    InsertAfter,
}

#[derive(Debug)]
pub struct OverlappingGroupOfMutations {
    pub kind: OverlappingGroupKind,
    pub start_line: LineNo,
    pub end_line: LineNo,
    pub mutations: Vec<MutationId>,
}

impl OverlappingGroupOfMutations {
    pub fn contains(&self, line: LineNo) -> bool {
        self.start_line <= line && line <= self.end_line
    }

    pub fn overlaps(&self, start_line: LineNo, end_line: LineNo) -> bool {
        self.start_line <= end_line && start_line <= self.end_line
    }
}

#[test]
fn test_overlapping_group_overlaps() {
    let group = OverlappingGroupOfMutations {
        kind: OverlappingGroupKind::Replace,
        start_line: LineNo(3),
        end_line: LineNo(5),
        mutations: vec![],
    };

    // Identical
    assert_eq!(true, group.overlaps(LineNo(3), LineNo(5)));

    // Inside
    assert_eq!(true, group.overlaps(LineNo(4), LineNo(4)));
    // Inside, touching start.
    assert_eq!(true, group.overlaps(LineNo(3), LineNo(4)));
    // Inside, touching end.
    assert_eq!(true, group.overlaps(LineNo(4), LineNo(5)));

    // Bottom overlap.
    assert_eq!(true, group.overlaps(LineNo(2), LineNo(3)));
    // Top overlap.
    assert_eq!(true, group.overlaps(LineNo(5), LineNo(6)));

    // Non-overlapping neighbor before.
    assert_eq!(false, group.overlaps(LineNo(1), LineNo(2)));
    // Non-overlapping neighbor after.
    assert_eq!(false, group.overlaps(LineNo(6), LineNo(7)));

    // Outside, before.
    assert_eq!(false, group.overlaps(LineNo(1), LineNo(1)));
    // Outside, after.
    assert_eq!(false, group.overlaps(LineNo(8), LineNo(9)));
}

pub fn update_overlapping_groups(groups_in_files: &mut HashMap<PathBuf, Vec<OverlappingGroupOfMutations>>, mutation: &mutest_json::mutations::Mutation) {
    'subst: for subst in &mutation.substs {
        let subst_span = subst.location.span();
        let (subst_group_kind, subst_start_line, subst_end_line) = match subst.location {
            mutest_json::mutations::SubstitutionLocation::InsertBefore(_) => {
                let line_no = LineNo(subst_span.begin.0 as u32);
                (OverlappingGroupKind::InsertBefore, line_no, line_no)
            }
            mutest_json::mutations::SubstitutionLocation::InsertAfter(_) => {
                let line_no = LineNo(subst_span.end.0 as u32);
                (OverlappingGroupKind::InsertAfter, line_no, line_no)
            }
            mutest_json::mutations::SubstitutionLocation::Replace(_) => {
                let subst_start_line = LineNo(subst_span.begin.0 as u32);
                let subst_end_line = LineNo(subst_span.end.0 as u32);
                (OverlappingGroupKind::Replace, subst_start_line, subst_end_line)
            }
        };

        let groups_in_file = groups_in_files.entry(subst_span.path.to_owned()).or_default();

        let mut groups_in_file_iter = groups_in_file.iter_mut().enumerate();
        while let Some((i, g)) = groups_in_file_iter.next() {
            // Found matching overlapping group; update group, and see if it now absorbs other groups.
            if g.kind == subst_group_kind && g.overlaps(subst_start_line, subst_end_line) {
                g.start_line = Ord::min(subst_start_line, g.start_line);
                g.end_line = Ord::max(subst_end_line, g.end_line);
                g.mutations.push(mutation.mutation_id);
                // FIXME: Absorb other, newly overlapping groups.
                continue 'subst;
            }

            // Found (the first) matching group that would come after this mutation substitution; insert a new group before.
            if g.start_line >= subst_start_line && g.kind > subst_group_kind {
                groups_in_file.insert(i, OverlappingGroupOfMutations {
                    kind: subst_group_kind,
                    start_line: subst_start_line,
                    end_line: subst_end_line,
                    mutations: vec![mutation.mutation_id],
                });
                continue 'subst;
            }
        }

        // Did not find previous overlapping or proceeding group; create new one at the end.
        groups_in_file.push(OverlappingGroupOfMutations {
            kind: subst_group_kind,
            start_line: subst_start_line,
            end_line: subst_end_line,
            mutations: vec![mutation.mutation_id],
        });
    }
}
