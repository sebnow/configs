{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        marksman
      ];
    };
    xdg.configFile."nvim/lsp/marksman.lua".source = ./marksman.lua;
  };
}
