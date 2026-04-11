{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.yaml-language-server
      ];
    };
  };
}
