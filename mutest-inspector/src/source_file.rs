use std::ffi::OsStr;
use std::iter;
use std::path::{self, Path, PathBuf};

use mutest_json::data_structures::{Idx, IdxVec};

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
