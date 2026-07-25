{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage {
  pname = "mdbase-tasknotes";
  version = "0.2.0-rc.1";

  src = fetchFromGitHub {
    owner = "callumalpass";
    repo = "mdbase-tasknotes";
    tag = "v0.2.0-rc.1";
    hash = "sha256-wBNeTJuR6pp9e4hblrmocsq3vVTJjk0r1UMYGzzbnAs=";
  };

  npmDepsHash = "sha256-j88BBNb//j5xg6OTO6WHUnCMMexc7vyQJQq+MvMSeT8=";

  meta = {
    description = "Standalone CLI for managing markdown tasks via mdbase";
    homepage = "https://github.com/callumalpass/mdbase-tasknotes";
    license = lib.licenses.mit;
    mainProgram = "mtn";
  };
}
