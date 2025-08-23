{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.nodePackages.yaml-language-server
      ];
    };
  };
}
