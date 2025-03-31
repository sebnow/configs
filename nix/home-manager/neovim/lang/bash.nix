{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.shellcheck
        pkgs.nodePackages.bash-language-server
      ];
    };
    xdg.configFile."nvim/lsp/bashls.lua".source = ./bashls.lua;
  };
}
