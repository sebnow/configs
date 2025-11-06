{
  stdenv,
  pkgs,
  ...
}: let
  tokenSimulationPlugin = pkgs.fetchFromGitHub {
    owner = "camunda";
    repo = "camunda-modeler-token-simulation-plugin";
    rev = "v0.20.0";
    sha256 = "OC6VFeIuC9p1OXbs/W3na8P0QSeTMkJr7fQeUNPOAEw=";
  };
in
  stdenv.mkDerivation
  rec {
    pname = "camunda-modeler";
    version = "5.28.0";
    src = pkgs.fetchzip {
      url = "https://github.com/camunda/${pname}/releases/download/v${version}/${pname}-${version}-linux-x64.tar.gz";
      sha256 = "S5TrJ+g9HqQcwyYwLJtjDaRXhztxFimfkY+kROE1ZcA=";
    };
    installPhase = ''
      ls -la $src;
      mkdir $out
      cp -r $src/* $out/
      mkdir -p $out/plugins;
      cp -r ${tokenSimulationPlugin} $out/plugins/camunda-modeler-token-simulation-plugin;
      mkdir -p $out/bin
      cat >> $out/bin/camunda-modeler <<EOF
#!${pkgs.bash}/bin/bash
cd $out && ./camunda-modeler
EOF
      chmod +x $out/bin/camunda-modeler;
    '';
  }
