{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.buf
      ];
    };
    xdg.configFile."nvim/lsp/buf_ls.lua".source = ./buf_ls.lua;
  };
}
