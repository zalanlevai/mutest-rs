use std::env;
use std::path::Path;

fn main() {
    let static_dir_path = Path::new("./src/static");
    let static_dir_path = static_dir_path.canonicalize().expect("cannot canonicalize static dir path");

    // NOTE: Force memory-serve not to print build-time log messages.
    // SAFETY: No other thread is running.
    unsafe { env::set_var("MEMORY_SERVE_QUIET", "1") };

    let embed_static_files = env::var("CARGO_FEATURE_EMBED_STATIC_FILES").is_ok();
    memory_serve::load_directory_with_embed(static_dir_path, embed_static_files);
}
