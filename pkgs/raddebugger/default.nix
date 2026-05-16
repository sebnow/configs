{
  lib,
  makeBinaryWrapper,
  pkgs,
  fetchFromGitHub,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "raddebugger";
  version = "0.9.25-alpha";

  src = fetchFromGitHub {
    owner = "EpicGamesExt";
    repo = "raddebugger";
    rev = "v${version}";
    hash = "sha256-JD/gksSg1MbozxJz9nOn96HHv8cBPlQRJ8Rxrzt8dy0=";
  };

  patches = [
    ./gcc15-force-inline-static.patch
  ];

  strictDeps = true;

  nativeBuildInputs = [
    pkgs.bash
    pkgs.coreutils
    pkgs.gnugrep
    pkgs.gnused
  ];

  buildInputs = [
    pkgs.freetype
    pkgs.xorg.libX11
    pkgs.xorg.libXext
    pkgs.libGL
  ];

  makeWrapperArgs = [
    "--prefix PATH: ${
      lib.makeBinPath [
        pkgs.llvm
      ]
    }"
  ];

  # GCC 15 defaults to -std=gnu23 where tentative + initializing definitions
  # in the same TU are redefinition errors. Upstream relies on C17 semantics.
  NIX_CFLAGS_COMPILE = "-std=gnu17";

  postPatch = ''
    substituteInPlace build.sh \
        --replace-fail 'git_hash=$(git describe --always --dirty)' 'git_hash=''${GIT_HASH:-unknown}' \
        --replace-fail 'git_hash_full=$(git rev-parse HEAD)' 'git_hash_full=''${GIT_HASH_FULL:-unknown}' \
        --replace-fail '-Wno-unused-local-typedef' "" \
        --replace-fail '-Wno-unknown-warning-option' ""
    substituteInPlace src/third_party/radsort/radsort.h \
        --replace-fail '#define RSFORCEINLINE __attribute__((always_inline))' '#define RSFORCEINLINE'
    chmod +x build.sh
  '';

  buildPhase = ''
    runHook preBuild
    bash ./build.sh gcc release raddbg
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin"
    install -Dm755 build/raddbg "$out/bin/raddbg"
    runHook postInstall
  '';

  meta = with lib; {
    description = "RAD Debugger";
    homepage = "https://github.com/EpicGamesExt/raddebugger";
    license = licenses.mit;
    platforms = platforms.linux;
    mainProgram = "raddbg";
  };
}
