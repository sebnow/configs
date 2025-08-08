{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        golangci-lint
        golangci-lint-langserver
        gopls
      ];
    };
    xdg.configFile."nvim/lsp/gopls.lua".source = ./gopls.lua;
    xdg.configFile."nvim/lsp/golangci_lint_ls.lua".source = ./golangci_lint_ls.lua;
  };
}
