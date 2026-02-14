{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.gofumpt
        pkgs.golangci-lint
        pkgs.golangci-lint-langserver
        pkgs.golines
        pkgs.gopls
      ];
    };
  };
}
