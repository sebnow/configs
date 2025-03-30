{pkgs, ...}: {
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        gopls
        golangci-lint-langserver
      ];
    };
    xdg.configFile."nvim/lsp/gopls.lua".source = ./gopls.lua;
    xdg.configFile."nvim/lsp/golangci_lint_ls.lua".source = ./golangci_lint_ls.lua;
  };
}
