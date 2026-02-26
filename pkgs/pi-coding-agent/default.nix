{
  lib,
  buildNpmPackage,
  fetchzip,
}:
buildNpmPackage (finalAttrs: {
  pname = "pi-coding-agent";
  version = "0.55.1";

  src = fetchzip {
    url = "https://registry.npmjs.org/@mariozechner/pi-coding-agent/-/pi-coding-agent-${finalAttrs.version}.tgz";
    hash = "sha256-Qjuv8GzMdENnNwX5xo/Yfh4aRhn7N7pIlXrkeoHGJMk=";
  };

  npmDepsHash = "sha256-sxFRcIT2FPn/7lb8KZ7/ivMCpdv5QqWFsbIQHU6p9RE=";

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
