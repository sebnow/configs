{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  openssl,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "lore";
  version = "0.8.4";

  src = fetchurl {
    url = "https://github.com/EpicGames/lore/releases/download/v${finalAttrs.version}/lore-v${finalAttrs.version}-x86_64-unknown-linux-gnu.tar.gz";
    hash = "sha256-sd+rhUqqK7tAeMAmYzGhBJ4lpeJ6WMSWNYCNCz2zObs=";
  };

  serverSrc = fetchurl {
    url = "https://github.com/EpicGames/lore/releases/download/v${finalAttrs.version}/loreserver-v${finalAttrs.version}-x86_64-unknown-linux-gnu.tar.gz";
    hash = "sha256-QrzkEk0HZC4OscmW1iYoiXYLFCmS0O0PYFTQlovJ78E=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [
    openssl
    stdenv.cc.cc.lib
  ];

  unpackPhase = ''
    runHook preUnpack
    mkdir source
    tar -xzf $src -C source
    tar -xzf $serverSrc -C source
    sourceRoot=source
    runHook postUnpack
  '';

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 $(find . -maxdepth 2 -name lore -type f) $out/bin/lore
    install -Dm755 $(find . -maxdepth 2 -name loreserver -type f) $out/bin/loreserver
    runHook postInstall
  '';

  meta = {
    description = "Next-generation version control system for projects with large binary assets";
    homepage = "https://github.com/EpicGames/lore";
    license = lib.licenses.mit;
    mainProgram = "lore";
    platforms = [ "x86_64-linux" ];
  };
})
