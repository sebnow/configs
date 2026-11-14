{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.gofumpt
        pkgs.golines
      ];
    };
  };
}
