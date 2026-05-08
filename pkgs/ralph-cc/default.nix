{
  lib,
  stdenv,
  makeWrapper,
}:
stdenv.mkDerivation {
  pname = "ralph-cc";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 ralph-cc "$out/bin/ralph-cc"
    install -Dm644 preamble.md "$out/share/ralph-cc/preamble.md"
    install -Dm644 epilogue.md "$out/share/ralph-cc/epilogue.md"
    wrapProgram "$out/bin/ralph-cc" \
      --set RALPH_CC_SHARE "$out/share/ralph-cc"
    runHook postInstall
  '';

  meta = {
    description = "Agentic loop driver for Claude Code";
    license = lib.licenses.mit;
    mainProgram = "ralph-cc";
  };
}
