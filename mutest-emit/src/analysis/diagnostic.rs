use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use rustc_data_structures::sync::Lrc;
use rustc_error_messages::{FluentBundle, LazyFallbackBundle};
use rustc_errors::{Diag, EmissionGuarantee, TerminalUrl};
use rustc_errors::emitter::{DynEmitter, HumanEmitter};
use rustc_session::Session;
use rustc_span::source_map::SourceMap;

pub fn escape_literal(s: &str) -> String {
    let mut escaped = String::with_capacity(s.len());
    let mut chrs = s.chars().peekable();
    while let Some(first) = chrs.next() {
        match (first, chrs.peek()) {
            ('\\', Some(&delim @ '"') | Some(&delim @ '\'')) => {
                escaped.push('\\');
                escaped.push(delim);
                chrs.next();
            }
            ('"' | '\'' | '\\', _) => {
                escaped.push('\\');
                escaped.push(first)
            }
            (c, _) => escaped.push(c),
        };
    }
    escaped
}

pub trait SessionRcSourceMap {
    fn rc_source_map(&self) -> Lrc<SourceMap>;
}

impl SessionRcSourceMap for Session {
    fn rc_source_map(&self) -> Lrc<SourceMap> {
        self.psess.clone_source_map()
    }
}

struct SharedBuffer<T: Write> {
    data: Arc<Mutex<T>>,
}

impl<T: Write> Write for SharedBuffer<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.data.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.data.lock().unwrap().flush()
    }
}

fn emit_with_emitter<G: EmissionGuarantee>(mut diagnostic: Diag<G>, emitter: Box<DynEmitter>) {
    let emit_dcx = rustc_errors::DiagCtxt::new(emitter);

    // Cast reference to the temporary diagnostic context to
    // the lifetime of the original context of the diagnostic.
    // SAFETY: The diagnostic will be consumed (and emitted) in this scope,
    //         before the temporary diagnostic context is dropped.
    diagnostic.dcx = unsafe { &*(&emit_dcx as *const _) };

    diagnostic.emit();
}

pub fn raw_output_full<G: EmissionGuarantee>(
    diagnostic: Diag<G>,
    source_map: Option<Lrc<SourceMap>>,
    fluent_bundle: Option<Lrc<FluentBundle>>,
    fallback_bundle: LazyFallbackBundle,
    short_message: bool,
    teach: bool,
    ui_testing: bool,
    ignored_directories_in_source_blocks: Vec<String>,
    diagnostic_width: Option<usize>,
    macro_backtrace: bool,
    track_diagnostics: bool,
    terminal_url: TerminalUrl,
) -> Vec<u8> {
    let output = Arc::new(Mutex::new(Vec::<u8>::new()));

    let dst = Box::new(termcolor::Ansi::new(SharedBuffer { data: output.clone() }));
    let emitter = HumanEmitter::new(dst, fallback_bundle)
        .sm(source_map)
        .fluent_bundle(fluent_bundle)
        .short_message(short_message)
        .teach(teach)
        .ui_testing(ui_testing)
        .ignored_directories_in_source_blocks(ignored_directories_in_source_blocks)
        .diagnostic_width(diagnostic_width)
        .macro_backtrace(macro_backtrace)
        .track_diagnostics(track_diagnostics)
        .terminal_url(terminal_url);

    emit_with_emitter(diagnostic, Box::new(emitter));

    Mutex::into_inner(Arc::try_unwrap(output).unwrap()).unwrap()
}

pub fn output_full<G: EmissionGuarantee>(
    diagnostic: Diag<G>,
    source_map: Option<Lrc<SourceMap>>,
    fluent_bundle: Option<Lrc<FluentBundle>>,
    fallback_bundle: LazyFallbackBundle,
    short_message: bool,
    teach: bool,
    ui_testing: bool,
    ignored_directories_in_source_blocks: Vec<String>,
    diagnostic_width: Option<usize>,
    macro_backtrace: bool,
    track_diagnostics: bool,
    terminal_url: TerminalUrl,
) -> String {
    let bytes = raw_output_full(diagnostic, source_map, fluent_bundle, fallback_bundle, short_message, teach, ui_testing, ignored_directories_in_source_blocks, diagnostic_width, macro_backtrace, track_diagnostics, terminal_url);
    String::from_utf8(bytes).unwrap()
}

pub fn output<G: EmissionGuarantee>(diagnostic: Diag<G>, source_map: Lrc<SourceMap>) -> String {
    let fluent_bundle = None;
    let fallback_bundle = rustc_errors::fallback_fluent_bundle(vec![], true);
    let short_message = false;
    let teach = false;
    let ui_testing = false;
    let ignored_directories_in_source_blocks = vec![];
    let diagnostic_width = None;
    let macro_backtrace = false;
    let track_diagnostics = false;
    let terminal_url = TerminalUrl::Yes;

    output_full(diagnostic, Some(source_map), fluent_bundle, fallback_bundle, short_message, teach, ui_testing, ignored_directories_in_source_blocks, diagnostic_width, macro_backtrace, track_diagnostics, terminal_url)
}

pub fn emit_str<G: EmissionGuarantee>(diagnostic: Diag<G>, source_map: Lrc<SourceMap>) -> String {
    output(diagnostic, source_map)
}
