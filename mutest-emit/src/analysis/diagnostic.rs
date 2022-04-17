use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use rustc_data_structures::sync::Lrc;
use rustc_error_messages::FluentBundle;
use rustc_errors::{Diagnostic, DiagnosticBuilder, EmissionGuarantee};
use rustc_errors::emitter::{Emitter, EmitterWriter};
use rustc_session::Session;
use rustc_span::source_map::SourceMap;

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
    fallback_bundle: Lrc<FluentBundle>,
    short_message: bool,
    teach: bool,
    colored: bool,
    terminal_width: Option<usize>,
    macro_backtrace: bool,
) -> Vec<u8> {
    let output = Arc::new(Mutex::new(Vec::<u8>::new()));

    let mut emitter = EmitterWriter::new(
        Box::new(SharedBuffer { data: output.clone() }),
        source_map,
        fluent_bundle,
        fallback_bundle,
        short_message,
        teach,
        colored,
        terminal_width,
        macro_backtrace,
    );

    emitter.emit_diagnostic(diagnostic);

    // The emitter is dropped explitictly to release all of its borrows of the shared output buffer.
    drop(emitter);

    Mutex::into_inner(Arc::try_unwrap(output).unwrap()).unwrap()
}

pub fn output_full(
    diagnostic: &Diagnostic,
    source_map: Option<Lrc<SourceMap>>,
    fluent_bundle: Option<Lrc<FluentBundle>>,
    fallback_bundle: Lrc<FluentBundle>,
    short_message: bool,
    teach: bool,
    colored: bool,
    terminal_width: Option<usize>,
    macro_backtrace: bool,
) -> String {
    let bytes = raw_output_full(diagnostic, source_map, fluent_bundle, fallback_bundle, short_message, teach, colored, terminal_width, macro_backtrace);
    String::from_utf8(bytes).unwrap()
}

pub fn output(diagnostic: &Diagnostic, source_map: Lrc<SourceMap>) -> String {
    let fluent_bundle = None;
    let fallback_bundle = rustc_errors::fallback_fluent_bundle(true).unwrap();
    let short_message = false;
    let teach = false;
    let colored = true;
    let terminal_width = None;
    let macro_backtrace = false;
    output_full(diagnostic, Some(source_map), fluent_bundle, fallback_bundle, short_message, teach, colored, terminal_width, macro_backtrace)
}

pub fn emit_str<G: EmissionGuarantee>(diagnostic: DiagnosticBuilder<G>, source_map: Lrc<SourceMap>) -> String {
    let (diagnostic, _) = diagnostic.into_diagnostic().unwrap();
    output(&diagnostic, source_map)
}
