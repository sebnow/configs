{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.clang-tools
      ];
    };
  };
}
