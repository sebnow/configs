{
  lib,
  buildNpmPackage,
  fetchzip,
  fd,
  ripgrep,
  makeBinaryWrapper,
}:
buildNpmPackage (finalAttrs: {
  pname = "pi-coding-agent";
  version = "0.61.1";

  src = fetchzip {
    url = "https://registry.npmjs.org/@mariozechner/pi-coding-agent/-/pi-coding-agent-${finalAttrs.version}.tgz";
    hash = "sha256-XhVH0WG1MqezyUnpnRgVoXnFgMRxq/6id0CSEpPCWEQ=";
  };

  npmDepsHash = "sha256-u0CJQC9ViH6QM010g2rA8aUBeY9u/350OX8Q8wln/38=";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  nativeBuildInputs = [ makeBinaryWrapper ];

  dontNpmBuild = true;

  postInstall = ''
    wrapProgram $out/bin/pi \
      --prefix PATH : ${
        lib.makeBinPath [
          fd
          ripgrep
        ]
      }
  '';

  meta = {
    description = "Coding agent CLI with read, bash, edit, write tools and session management";
    homepage = "https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent";
    license = lib.licenses.mit;
    mainProgram = "pi";
  };
})
