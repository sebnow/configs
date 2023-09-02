{
  config,
  pkgs,
  lib,
  stdenv,
  ...
}: let
  cfg = config.programs.k9s;
  catppuccin = pkgs.fetchFromGitHub {
    owner = "catppuccin";
    repo = "k9s";
    rev = "516f44dd1a6680357cb30d96f7e656b653aa5059";
    sha256 = "sha256-PtBJRBNbLkj7D2ko7ebpEjbfK9Ywjs7zbE+Y8FQVEfA=";
  };
in {
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      xdg.configFile."k9s/skin.yml" = {
        source = catppuccin + "/dist/mocha.yml";
      };
    })
  ];
}
