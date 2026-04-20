use std::ffi::OsStr;
use std::iter;
use std::path::{self, Path, PathBuf};

use mutest_json::Span;
use mutest_json::data_structures::{Idx, IdxVec, IdxSlice};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct LineNo(pub u32);

impl Idx for LineNo {
    fn as_index(self) -> usize {
        self.0 as usize - 1
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32 + 1)
    }
}

pub struct SourceFile {
    pub lines: IdxVec<LineNo, String>,
}

pub fn nudge_span_prefix_lines(lines: &IdxSlice<LineNo, String>, span: &Span) -> usize {
    let start_line_no = LineNo(span.begin.0 as u32);

    // NOTE: If span is preceeded by anything else in the same line, then we do not add prefix lines to it,
    //       as they might pertain to the preceeding tokens.
    // FIXME: This heuristic does not work for oddly formatted code, such as `#[test]\n#[ignore] fn test()`.
    if !lines[start_line_no].chars().take(span.begin.1 - 1).all(char::is_whitespace) { return 0; }

    lines[..start_line_no].iter().rev()
        .take_while(|line| line.trim().starts_with("#["))
        .count()
}

/// Returns the start and end byte offsets of the region of the specified line that falls within the span.
pub fn line_region_byte_offsets_within_span(line_no: LineNo, line: &str, span: &Span) -> Option<(usize, usize)> {
    let line_no = line_no.0 as usize;

    if line_no == span.begin.0 && line_no == span.end.0 {
        let start_line_start_byte_offset = {
            let mut char_indices_iter = line.char_indices();
            for _ in 1..span.begin.1 { char_indices_iter.next(); }
            char_indices_iter.offset()
        };
        let end_line_end_byte_offset = {
            let mut char_indices_iter = line.char_indices();
            for _ in 1..span.end.1 { char_indices_iter.next(); }
            char_indices_iter.offset()
        };

        return Some((start_line_start_byte_offset, end_line_end_byte_offset));
    }

    if line_no == span.begin.0 {
        let start_line_start_byte_offset = {
            let mut char_indices_iter = line.char_indices();
            for _ in 1..span.begin.1 { char_indices_iter.next(); }
            char_indices_iter.offset()
        };

        return Some((start_line_start_byte_offset, line.len()));
    }
    if line_no == span.end.0 {
        let end_line_end_byte_offset = {
            let mut char_indices_iter = line.char_indices();
            for _ in 1..span.end.1 { char_indices_iter.next(); }
            char_indices_iter.offset()
        };

        return Some((0, end_line_end_byte_offset));
    }

    if line_no > span.begin.0 && line_no < span.end.0 {
        return Some((0, line.len()));
    }

    None
}

#[derive(PartialEq, Eq, Debug)]
pub enum TreeEntry<'a> {
    Dir(&'a Path),
    File(&'a Path),
    EndDir,
}

pub fn file_tree_entries<'a>(file_paths: &'a[PathBuf]) -> impl Iterator<Item = TreeEntry<'a>> {
    assert!(file_paths.is_sorted(), "file paths are not sorted");

    let mut file_path_cursor = 0;
    let mut dir_cursor = Vec::<&OsStr>::new();

    iter::from_fn(move || {
        let Some(file_path) = file_paths.get(file_path_cursor) else {
            let _ = dir_cursor.pop()?;
            return Some(TreeEntry::EndDir);
        };

        let mut file_path_components = file_path.components();

        for dir in &dir_cursor {
            let Some(component) = file_path_components.next() else {
                dir_cursor.pop();
                return Some(TreeEntry::EndDir);
            };

            if component != path::Component::Normal(dir) {
                dir_cursor.pop();
                return Some(TreeEntry::EndDir);
            }
        }

        let Some(component) = file_path_components.next() else { unreachable!() };

        let normal_component = match component {
            path::Component::Prefix(_) => panic!("encountered path `{}` with prefix component", file_path.display()),
            path::Component::RootDir => panic!("encountered path `{}` with root directory component", file_path.display()),
            path::Component::CurDir => panic!("encountered path `{}` with current directory component", file_path.display()),
            path::Component::ParentDir => panic!("encountered path `{}` with parent directory component", file_path.display()),
            path::Component::Normal(normal_component) => normal_component,
        };

        let remaining_components_count = file_path_components.count();

        let Some(path) = file_path.ancestors().skip(remaining_components_count).next() else { unreachable!() };

        // NOTE: We can assume that this component is a file component if it is the last one,
        //       because we are explicitly dealing with file paths.
        if remaining_components_count >= 1 {
            dir_cursor.push(normal_component);
            return Some(TreeEntry::Dir(path));
        }

        file_path_cursor += 1;
        return Some(TreeEntry::File(path));
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_span(lo_line: usize, lo_col: usize, hi_line: usize, hi_col: usize) -> Span {
        Span { path: PathBuf::new(), begin: (lo_line, lo_col), end: (hi_line, hi_col) }
    }

    #[test]
    fn test_nudge_span_prefix_lines() {
        // No preceeding lines.
        let mut lines = IdxVec::new();
        lines.push("    fn test() {".to_owned());
        assert_eq!(0, nudge_span_prefix_lines(&lines, &dummy_span(1, 5, 1, 12)));

        // No preceeding attributes.
        let mut lines = IdxVec::new();
        lines.push("    const OTHER: () = ();".to_owned());
        lines.push("".to_owned());
        lines.push("    fn test() {".to_owned());
        assert_eq!(0, nudge_span_prefix_lines(&lines, &dummy_span(3, 5, 3, 12)));

        // Span is preceeded by other tokens on the same line.
        let mut lines = IdxVec::new();
        lines.push("    #[test]".to_owned());
        lines.push("    fn test() { let _ = || ();".to_owned());
        assert_eq!(0, nudge_span_prefix_lines(&lines, &dummy_span(2, 25, 2, 27)));

        // Single preceeding attribute.
        let mut lines = IdxVec::new();
        lines.push("    #[test]".to_owned());
        lines.push("    fn test() {".to_owned());
        assert_eq!(1, nudge_span_prefix_lines(&lines, &dummy_span(2, 5, 2, 12)));

        // Multiple preceeding attributes.
        let mut lines = IdxVec::new();
        lines.push("    #[test]".to_owned());
        lines.push("    #[ignore]".to_owned());
        lines.push("    fn test() {".to_owned());
        assert_eq!(2, nudge_span_prefix_lines(&lines, &dummy_span(3, 5, 3, 12)));

        // Preceeding attribute applies to the parent, not the node it preceeds.
        let mut lines = IdxVec::new();
        lines.push("    #![cfg(test)]".to_owned());
        lines.push("    #[test]".to_owned());
        lines.push("    fn test() {".to_owned());
        assert_eq!(1, nudge_span_prefix_lines(&lines, &dummy_span(3, 5, 3, 12)));

        // Span is preceeded by attributes, comments, and doc comments.
        // NOTE: Comments and doc comments are ignored because they can sometimes span a lot of lines.
        let mut lines = IdxVec::new();
        lines.push("".to_owned());
        lines.push("    /// Documentation.".to_owned());
        lines.push("    // Comment.".to_owned());
        lines.push("    #[cfg(feature = \"foo\")]".to_owned());
        lines.push("    fn foo() {".to_owned());
        assert_eq!(1, nudge_span_prefix_lines(&lines, &dummy_span(5, 5, 5, 12)));
    }

    #[test]
    fn test_line_region_byte_offsets_within_span() {
        // Empty string.
        assert_eq!(Some((0, 0)), line_region_byte_offsets_within_span(LineNo(1), "", &dummy_span(1, 1, 1, 1)));

        // Span selecting part of a line.
        assert_eq!(Some((2, 5)), line_region_byte_offsets_within_span(LineNo(1), "abcdef", &dummy_span(1, 3, 1, 6)));
        // Partially out of bounds span.
        assert_eq!(Some((0, 3)), line_region_byte_offsets_within_span(LineNo(1), "abc", &dummy_span(1, 1, 1, 5)));
        // Completely out of bounds span.
        assert_eq!(Some((3, 3)), line_region_byte_offsets_within_span(LineNo(1), "abc", &dummy_span(1, 4, 1, 5)));
        // Span selecting a different line.
        assert_eq!(None, line_region_byte_offsets_within_span(LineNo(3), "abc", &dummy_span(1, 1, 2, 15)));

        // First line of multiline span.
        assert_eq!(Some((3, 7)), line_region_byte_offsets_within_span(LineNo(1), "abc def", &dummy_span(1, 4, 3, 1)));
        // Last line of multiline span.
        assert_eq!(Some((0, 1)), line_region_byte_offsets_within_span(LineNo(5), "};", &dummy_span(3, 23, 5, 2)));
        // In-between line of multiline span.
        assert_eq!(Some((0, 8)), line_region_byte_offsets_within_span(LineNo(8), "    foo,", &dummy_span(6, 19, 9, 1)));
    }

    #[test]
    fn test_file_tree_entries() {
        let file_paths = &[
            PathBuf::from("src/lib.rs"),
            PathBuf::from("src/mod/nested/mod.rs"),
            PathBuf::from("src/mod/nested/other.rs"),
            PathBuf::from("src/other/src.rs"),
            PathBuf::from("src/root.rs"),
        ];

        let mut entries = file_tree_entries(file_paths);
        assert_eq!(Some(TreeEntry::Dir(&Path::new("src"))), entries.next());
        assert_eq!(Some(TreeEntry::File(&Path::new("src/lib.rs"))), entries.next());
        assert_eq!(Some(TreeEntry::Dir(&Path::new("src/mod"))), entries.next());
        assert_eq!(Some(TreeEntry::Dir(&Path::new("src/mod/nested"))), entries.next());
        assert_eq!(Some(TreeEntry::File(&Path::new("src/mod/nested/mod.rs"))), entries.next());
        assert_eq!(Some(TreeEntry::File(&Path::new("src/mod/nested/other.rs"))), entries.next());
        assert_eq!(Some(TreeEntry::EndDir), entries.next());
        assert_eq!(Some(TreeEntry::EndDir), entries.next());
        assert_eq!(Some(TreeEntry::Dir(&Path::new("src/other"))), entries.next());
        assert_eq!(Some(TreeEntry::File(&Path::new("src/other/src.rs"))), entries.next());
        assert_eq!(Some(TreeEntry::EndDir), entries.next());
        assert_eq!(Some(TreeEntry::File(&Path::new("src/root.rs"))), entries.next());
        assert_eq!(Some(TreeEntry::EndDir), entries.next());
        assert_eq!(None, entries.next());
    }
}
