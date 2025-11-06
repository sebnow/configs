{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.nixd
        pkgs.nixfmt-rfc-style
      ];
    };
  };
}
