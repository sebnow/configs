{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        nodePackages.yaml-language-server
      ];
    };
    xdg.configFile."nvim/lsp/yamlls.lua".source = ./yamlls.lua;
  };
}
