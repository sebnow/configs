{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        zig
        zls
      ];
    };
    xdg.configFile."nvim/lsp/zls.lua".source = ./zls.lua;
  };
}
