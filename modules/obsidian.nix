{ inputs, ... }:
{
  flake.modules.homeManager.obsidian =
    { config, pkgs, ... }:
    let
      # Obsidian renders a blank window on Electron 43.8.3: the renderer fails
      # in Electron's own web-frame.ts init ("Cannot read properties of
      # undefined (reading 'defineProperty')"). The same app works on 43.7.11.
      # Drop this override and the nixpkgs-electron-43-7 input once the
      # unpinned pkgs.obsidian opens a vault again.
      electron =
        inputs.nixpkgs-electron-43-7.legacyPackages.${pkgs.stdenv.hostPlatform.system}.electron;
      package = pkgs.obsidian.override { inherit electron; };
      obsidian = if pkgs.stdenv.isLinux then config.lib.nixGL.wrap package else package;
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
