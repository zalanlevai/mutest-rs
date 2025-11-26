use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use rustc_errors::{AutoStream, ColorChoice, Diag, DiagCtxt, EmissionGuarantee, TerminalUrl};
use rustc_errors::emitter::{ColorConfig, Destination, DynEmitter, HumanEmitter, OutputTheme};
use rustc_errors::translation::Translator;
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
    let emit_dcx = DiagCtxt::new(emitter);

    // Cast reference to the temporary diagnostic context to
    // the lifetime of the original context of the diagnostic.
    // SAFETY: The diagnostic will be consumed (and emitted) in this scope,
    //         before the temporary diagnostic context is dropped.
    let emit_dcx_ref: &DiagCtxt = unsafe { &*(&emit_dcx as *const _) };
    diagnostic.dcx = emit_dcx_ref.handle();

    diagnostic.emit();
}

pub fn raw_output_full<G: EmissionGuarantee>(
    diagnostic: Diag<G>,
    source_map: Option<Arc<SourceMap>>,
    translator: Translator,
    short_message: bool,
    ui_testing: bool,
    ignored_directories_in_source_blocks: Vec<String>,
    diagnostic_width: Option<usize>,
    macro_backtrace: bool,
    track_diagnostics: bool,
    terminal_url: TerminalUrl,
    theme: OutputTheme,
    color_config: ColorConfig,
) -> Vec<u8> {
    let output = Arc::new(Mutex::new(Vec::<u8>::new()));

    let shared_buffer = SharedBuffer { data: output.clone() };
    // NOTE: Cargo always requests colored JSON output, and pipes both stdout and stderr.
    //       This effectively disallows us from determining support for colored output automatically.
    //       Because of this, we can only respect the explicit `--color=never` flag, and
    //       print colors when the default `--color=auto` is specified.
    // NOTE: Similar to Cargo, we could always embed colors, and strip them during printing,
    //       if needed, using `anstream::adapter::strip_str`.
    //       However, this would require mutest_runtime to depend on anstream.
    let color_choice = match color_config {
        ColorConfig::Never => ColorChoice::Never,
        _ => ColorChoice::AlwaysAnsi,
    };
    let dst: Destination = AutoStream::new(Box::new(shared_buffer), color_choice);
    let emitter = HumanEmitter::new(dst, translator)
        .sm(source_map)
        .short_message(short_message)
        .ui_testing(ui_testing)
        .ignored_directories_in_source_blocks(ignored_directories_in_source_blocks)
        .diagnostic_width(diagnostic_width)
        .macro_backtrace(macro_backtrace)
        .track_diagnostics(track_diagnostics)
        .terminal_url(terminal_url)
        .theme(theme);

    emit_with_emitter(diagnostic, Box::new(emitter));

    Mutex::into_inner(Arc::try_unwrap(output).unwrap()).unwrap()
}

pub fn output_full<G: EmissionGuarantee>(
    diagnostic: Diag<G>,
    source_map: Option<Arc<SourceMap>>,
    translator: Translator,
    short_message: bool,
    ui_testing: bool,
    ignored_directories_in_source_blocks: Vec<String>,
    diagnostic_width: Option<usize>,
    macro_backtrace: bool,
    track_diagnostics: bool,
    terminal_url: TerminalUrl,
    theme: OutputTheme,
    color_config: ColorConfig,
) -> String {
    let bytes = raw_output_full(diagnostic, source_map, translator, short_message, ui_testing, ignored_directories_in_source_blocks, diagnostic_width, macro_backtrace, track_diagnostics, terminal_url, theme, color_config);
    String::from_utf8(bytes).unwrap()
}

pub fn output<G: EmissionGuarantee>(diagnostic: Diag<G>, sess: &Session) -> String {
    let source_map = Some(sess.psess.clone_source_map());
    let translator = rustc_driver::default_translator();
    let short_message = false;
    let ui_testing = sess.opts.unstable_opts.ui_testing;
    let ignored_directories_in_source_blocks = vec![];
    let diagnostic_width = sess.opts.diagnostic_width;
    let macro_backtrace = false;
    let track_diagnostics = false;
    let terminal_url = TerminalUrl::Yes;
    let theme = OutputTheme::Ascii;
    let color_config = sess.opts.color;

    output_full(diagnostic, source_map, translator, short_message, ui_testing, ignored_directories_in_source_blocks, diagnostic_width, macro_backtrace, track_diagnostics, terminal_url, theme, color_config)
}

pub fn emit_str<G: EmissionGuarantee>(diagnostic: Diag<G>, sess: &Session) -> String {
    output(diagnostic, sess)
}
