use std::path::Path;

fn main() {
    let static_dir_path = Path::new("./src/static");
    let static_dir_path = static_dir_path.canonicalize().expect("cannot canonicalize static dir path");

    println!("cargo:rustc-env=STATIC_DIR={}", static_dir_path.display());
}
