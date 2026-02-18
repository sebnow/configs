{
  lib,
  buildNpmPackage,
  fetchzip,
}:
buildNpmPackage (finalAttrs: {
  pname = "pi-coding-agent";
  version = "0.53.0";

  src = fetchzip {
    url = "https://registry.npmjs.org/@mariozechner/pi-coding-agent/-/pi-coding-agent-${finalAttrs.version}.tgz";
    hash = "sha256-/r/qGxNFGfE5+xul6QJucT6OuBuwzu2jM5xu4H3ThPs=";
  };

  npmDepsHash = "sha256-lZ0a0X7ysOH3I6PPpWuw61F0cnkguVUjmXxb8EbjEy8=";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  meta = {
    description = "Coding agent CLI with read, bash, edit, write tools and session management";
    homepage = "https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent";
    license = lib.licenses.mit;
    mainProgram = "pi";
  };
})
