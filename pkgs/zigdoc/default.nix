{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchzip,
  zig,
}:
let
  ziglint = fetchzip {
    url = "https://github.com/rockorager/ziglint/archive/refs/tags/v0.5.2.tar.gz";
    hash = "sha256-Q+iJ4vTqIm8FatvW8GdkmPEkIn7AEoMHHIhoWy+eYMs=";
  };
in
stdenv.mkDerivation {
  pname = "zigdoc";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "rockorager";
    repo = "zigdoc";
    tag = "v0.3.0";
    hash = "sha256-MhZ7LCsqZhLazDYwDZ/hzk9lYM3Bm1j96HDQ/OrdZFg=";
  };

  nativeBuildInputs = [ zig ];

  dontUseZigCheck = true;

  postConfigure = ''
    mkdir -p $ZIG_GLOBAL_CACHE_DIR/p
    ln -s ${ziglint} $ZIG_GLOBAL_CACHE_DIR/p/ziglint-0.5.2-t0bwL2FwBQC5i-ifhKKv2ls5jGXHShJpuNhFVhiU-Tt-
  '';

  meta = {
    description = "Terminal documentation viewer for Zig standard library and project dependencies";
    homepage = "https://github.com/rockorager/zigdoc";
    license = lib.licenses.mit;
    mainProgram = "zigdoc";
  };
}
