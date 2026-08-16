{
  lib,
  fetchFromGitHub,
  makeRustPlatform,
  makeWrapper,
}:
with import <nixpkgs> {
  overlays = [
    (import (fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
  ];
};
let
  rustPlatform = makeRustPlatform {
    cargo = rust-bin.nightly."2025-11-26".default.override {
      extensions = [
        "rust-std"
        "rust-src"
        "rustc-dev"
        "llvm-tools"
        "llvm-tools-preview"
      ];
    };
    rustc = rust-bin.nightly."2025-11-26".default.override {
      extensions = [
        "rust-std"
        "rust-src"
        "rustc-dev"
        "llvm-tools"
        "llvm-tools-preview"
      ];
    };
  };

  src = ./.;
in

rustPlatform.buildRustPackage {
  pname = "mutest-rs";
  version = "0.0.0";

  inherit src;

  cargoLock.lockFile = "${src}/Cargo.lock";

  patches = [ ./patch ];

  nativeBuildInputs = [
    makeWrapper
    libz
  ];

  buildPhase = ''
    runHook preBuild

    # Build mutest-runtime, needed to be built first due to build steps in cargo-mutest build
    # TODO I think cargo-mutest is just copying in the paths but we override it with MUTEST_SEARCH_PATH in the postInstall.wrapProgram phase,
    # maybe we can add a flag to tell cargo-mutest to ignore not finding runtime...
    # if we can then we could build mutest-runtime in a separate file and skip this whole step only linking at postInstall
    cargo build --offline --release --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package mutest-runtime

    # Due to how mutest-rs looks for the mutest-runtime deps it expects them all to be in 1 folder, for some reason when you specify target when building
    # cargo or rustc puts some things in ./target/release and other things in ./target/<rustcTargetSpec>/release
    # so I just copy it all across for it to compile :D
    cp target/release/deps/* target/${stdenv.targetPlatform.rust.rustcTargetSpec}/release/deps/

    # Build cargo mutest
    cargo build --offline --release --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package mutest-driver
    cargo build --offline --release --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package cargo-mutest

    runHook postBuild
  '';

  postInstall = ''
    # The end binary needs the .d and .rlib files as it uses them like a library but cargoInstallHook only copies files with these extensions (so.[0-9.]+\|so\|a\|dylib\)
    # TODO I can probably reduce the files in the lib folder for now im copying everything, see above TODO for better fix
    mkdir -p $out/lib/
    cp -r target/${stdenv.targetPlatform.rust.rustcTargetSpec}/release/* $out/lib

    wrapProgram $out/bin/cargo-mutest \
      --set MUTEST_SEARCH_PATH "$out/lib" \
      --prefix PATH : ${
        lib.makeBinPath [
          pkgs.rust-bin.nightly."2025-11-26".default
          pkgs.gcc
        ]
      }
  '';

  checkPhase = ''
    runHook preCheck

    cargo test --offline --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package cargo-mutest
    cargo test --offline --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package mutest-runtime
    cargo test --offline --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package mutest-inspector
    cargo test --offline --target ${stdenv.targetPlatform.rust.rustcTargetSpec} --package mutest-tests

    runHook postCheck
  '';

  meta = {
    description = "Mutation testing tools for Rust";
    longDescription = ''
      Generate and analyze runtime-swappable code mutants of Rust programs
      using a dynamic set of abstract mutation operators. For more information,
      see [the mutest-rs book](https://mutest.rs/).
    '';
    homepage = "https://github.com/zalanlevai/mutest-rs";
    changelog = "https://github.com/zalanlevai/mutest-rs/commits/main";
    license = with lib.licenses; [
      asl20
      mit
    ];
    maintainers = [ seam345 ];
    platforms = lib.platforms.unix;
  };
}
