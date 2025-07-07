#![cfg_attr(feature = "rustc", feature(rustc_private))]

use std::path::PathBuf;

use serde::{Serialize, Deserialize};

#[cfg(feature = "rustc")]
extern crate rustc_session;
#[cfg(feature = "rustc")]
extern crate rustc_span;

pub mod data_structures;
pub use data_structures::*;

/// Version number of the format described by this version of the crate.
///
/// Consumers of mutest-rs JSON data must validate that
/// the format version of the JSON data matches
/// the format version described by this crate.
pub const FORMAT_VERSION: u32 = 1;

/// A range of source code.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct Span {
    pub path: PathBuf,
    pub begin: (usize, usize),
    pub end: (usize, usize),
}

#[cfg(feature = "rustc")]
impl Span {
    pub fn from_rustc_span(sess: &rustc_session::Session, span: rustc_span::Span) -> Option<Self> {
        let (Some(source_file), begin_line, begin_col, end_line, end_col) = sess.source_map().span_to_location_info(span) else { return None; };

        let rustc_span::FileName::Real(file_name) = &source_file.name else { return None; };
        let path = file_name.local_path()?.to_owned();

        Some(Self { path, begin: (begin_line, begin_col), end: (end_line, end_col) })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Safety {
    Safe,
    Unsafe,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct DefId(pub u32);

impl Idx for DefId {
    fn as_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct Definition {
    pub def_id: DefId,
    pub name: Option<String>,
    pub path: Option<String>,
    pub span: Option<Span>,
}

pub mod call_graph;
pub mod evaluation;
pub mod evaluation_stream;
pub mod tests;
pub mod timings;
pub mod mutations;
