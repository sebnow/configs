{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.shellcheck
        pkgs.bash-language-server
      ];
    };
  };
}
