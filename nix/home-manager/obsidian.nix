{
  lib,
  pkgs,
  ...
}: {
  home.packages =
    if pkgs.stdenv.isLinux
    then [
      (pkgs.obsidian.overrideAttrs (prev: rec {
        desktopItem = prev.desktopItem.override {
          # https://pandasauce.org/post/linux-fonts/#application-settings
          exec = "obsidian --disable-font-subpixel-positioning %U";
        };
        installPhase = builtins.replaceStrings ["${prev.desktopItem}"] ["${desktopItem}"] prev.installPhase;
      }))
    ]
    else [pkgs.obsidian];
}
