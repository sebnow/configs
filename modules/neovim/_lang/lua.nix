{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.lua-language-server
        pkgs.stylua
      ];
    };
  };
}
