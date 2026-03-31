{ ... }:
{
  flake.modules.homeManager.obsidian =
    { config, pkgs, ... }:
    let
      obsidian =
        if pkgs.stdenv.isLinux then
          config.lib.nixGL.wrap pkgs.obsidian
        else
          pkgs.obsidian;
    in
    {
      home.packages = [ obsidian ];

      xdg.dataFile."applications/obsidian.desktop".text = ''
        [Desktop Entry]
        Version=1.5
        Type=Application
        Name=Obsidian
        Comment=Knowledge base
        Exec=obsidian --disable-font-subpixel-positioning %U
        Icon=obsidian
        Categories=Office;
        MimeType=x-scheme-handler/obsidian;
      '';
    };
}
