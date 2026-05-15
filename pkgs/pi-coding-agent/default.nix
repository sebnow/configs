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
  version = "0.73.1";

  src = fetchzip {
    url = "https://registry.npmjs.org/@mariozechner/pi-coding-agent/-/pi-coding-agent-${finalAttrs.version}.tgz";
    hash = "sha256-ZBSiOoKig+TGR7tswMro9CCmrQ5AI6Yf21XkBFastao=";
  };

  npmDepsHash = "sha256-WE0nUfTQubtLAixkymjYrbD8beDrp3D/YQyTic1PW+E=";

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
