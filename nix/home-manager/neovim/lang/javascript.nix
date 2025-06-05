{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        typescript-language-server
        prettierd
      ];
    };
    xdg.configFile."nvim/lsp/tsserver.lua".source = ./tsserver.lua;
  };
}
