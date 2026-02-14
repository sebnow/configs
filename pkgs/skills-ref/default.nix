{
  lib,
  fetchFromGitHub,
  buildPythonApplication,
  hatchling,
  click,
  strictyaml,
}:
buildPythonApplication {
  pname = "skills-ref";
  version = "0.1.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "agentskills";
    repo = "agentskills";
    rev = "e81550d";
    hash = "sha256-DoqTLslAzPGSXTMemU7hyRYEGU8xLn8PYQXnqd071o4=";
  };
  sourceRoot = "source/skills-ref";

  build-system = [ hatchling ];
  dependencies = [
    click
    strictyaml
  ];

  meta = {
    description = "Reference library for Agent Skills";
    homepage = "https://github.com/agentskills/agentskills";
    license = lib.licenses.asl20;
    mainProgram = "skills-ref";
  };
}
