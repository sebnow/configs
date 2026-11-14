{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.prettier
        pkgs.prettierd
      ];
    };
  };
}
