{ pkgs, ... }:
{
  config = {
    programs.neovim = {
      extraPackages = [
        pkgs.zig
        pkgs.zls
      ];
    };
  };
}
