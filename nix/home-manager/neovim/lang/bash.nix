{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.shellcheck
        pkgs.nodePackages.bash-language-server
      ];
    };
  };
}
