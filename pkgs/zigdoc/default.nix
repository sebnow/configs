{
  lib,
  stdenv,
  fetchFromGitHub,
  # zigdoc v0.3.0 requires zig >=0.15.1 and is not yet compatible with 0.16
  zig_0_15,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "zigdoc";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "rockorager";
    repo = "zigdoc";
    tag = "v0.3.0";
    hash = "sha256-MhZ7LCsqZhLazDYwDZ/hzk9lYM3Bm1j96HDQ/OrdZFg=";
  };

  zigDeps = zig_0_15.fetchDeps {
    inherit (finalAttrs) src pname version;
    hash = "sha256-+ifmpS+r7vbGncJlnEUvg8vsaNv3koKdizs9r31QmwE=";
  };

  nativeBuildInputs = [ zig_0_15.hook ];

  postConfigure = ''
    ln -s ${finalAttrs.zigDeps} "$ZIG_GLOBAL_CACHE_DIR/p"
  '';

  dontUseZigCheck = true;

  meta = {
    description = "Terminal documentation viewer for Zig standard library and project dependencies";
    homepage = "https://github.com/rockorager/zigdoc";
    license = lib.licenses.mit;
    mainProgram = "zigdoc";
  };
})
