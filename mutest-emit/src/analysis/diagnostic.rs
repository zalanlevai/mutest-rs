use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use rustc_data_structures::sync::Lrc;
use rustc_error_messages::{FluentBundle, LazyFallbackBundle};
use rustc_errors::{Diagnostic, DiagnosticBuilder, EmissionGuarantee};
use rustc_errors::emitter::{Emitter, EmitterWriter};
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
            ('"' | '\'', _) => {
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
        self.parse_sess.clone_source_map()
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

pub fn raw_output_full(
    diagnostic: &Diagnostic,
    source_map: Option<Lrc<SourceMap>>,
    fluent_bundle: Option<Lrc<FluentBundle>>,
    fallback_bundle: LazyFallbackBundle,
    short_message: bool,
    teach: bool,
    diagnostic_width: Option<usize>,
    macro_backtrace: bool,
    track_diagnostics: bool,
) -> Vec<u8> {
    let output = Arc::new(Mutex::new(Vec::<u8>::new()));

    let dst = Box::new(termcolor::Ansi::new(SharedBuffer { data: output.clone() }));
    let mut emitter = EmitterWriter::new(dst, fallback_bundle)
        .sm(source_map)
        .fluent_bundle(fluent_bundle)
        .short_message(short_message)
        .teach(teach)
        .diagnostic_width(diagnostic_width)
        .macro_backtrace(macro_backtrace)
        .track_diagnostics(track_diagnostics);

    emitter.emit_diagnostic(diagnostic);

    // The emitter is dropped explitictly to release all of its borrows of the shared output buffer.
    drop(emitter);

    Mutex::into_inner(Arc::try_unwrap(output).unwrap()).unwrap()
}

pub fn output_full(
    diagnostic: &Diagnostic,
    source_map: Option<Lrc<SourceMap>>,
    fluent_bundle: Option<Lrc<FluentBundle>>,
    fallback_bundle: LazyFallbackBundle,
    short_message: bool,
    teach: bool,
    diagnostic_width: Option<usize>,
    macro_backtrace: bool,
    track_diagnostics: bool,
) -> String {
    let bytes = raw_output_full(diagnostic, source_map, fluent_bundle, fallback_bundle, short_message, teach, diagnostic_width, macro_backtrace, track_diagnostics);
    String::from_utf8(bytes).unwrap()
}

pub fn output(diagnostic: &Diagnostic, source_map: Lrc<SourceMap>) -> String {
    let fluent_bundle = None;
    let fallback_bundle = rustc_errors::fallback_fluent_bundle(vec![], true);
    let short_message = false;
    let teach = false;
    let diagnostic_width = None;
    let macro_backtrace = false;
    let track_diagnostics = false;

    output_full(diagnostic, Some(source_map), fluent_bundle, fallback_bundle, short_message, teach, diagnostic_width, macro_backtrace, track_diagnostics)
}

pub fn emit_str<G: EmissionGuarantee>(diagnostic: DiagnosticBuilder<G>, source_map: Lrc<SourceMap>) -> String {
    let (diagnostic, _) = diagnostic.into_diagnostic().unwrap();
    output(&diagnostic, source_map)
}
