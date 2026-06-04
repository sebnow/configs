{ config, inputs, ... }:
let
  inherit (config.flake.modules) homeManager;
in
{
  configurations.homeManager."sebnow@stribog" = {
    system = "x86_64-linux";
    module =
      { pkgs, lib, ... }:
      {
        imports = [
          homeManager.agentic
          homeManager.camunda-modeler
          homeManager.catppuccin
          homeManager.fonts
          homeManager.gnome
          homeManager.go
          homeManager.ipfs
          homeManager.mlflow
          homeManager.neovim
          homeManager.niri
          homeManager.obsidian
          homeManager.shell
          homeManager.source-control
          homeManager.terminals
          homeManager.tmux
        ];

        nixpkgs.config = {
          allowUnfreePredicate =
            pkg:
            builtins.elem (lib.getName pkg) [
              "claude-code"
              "obsidian"
            ];
          permittedInsecurePackages = [
            "electron-25.9.0" # Used by Obsidian
          ];
        };

        home.file.".face".source = pkgs.fetchurl {
          url = "https://blossom.vlk.sh/90c7d6c27ac628382544fdd18e3ceb0a0e7b6db9479a9dea8a71ac9d7d19c33c";
          hash = "sha256-kMfWwnrGKDglRP3RjjzrCg57bblHmp3qinGsnX0Zwzw=";
        };

        home.file."Pictures/Wallpapers/neon-wolf-1a8c6d1b.png".source = pkgs.fetchurl {
          url = "https://blossom.vlk.sh/1a8c6d1b275b146a2b9e8abaa36a555d8cd6faf52bbdad85d688fb93314ce6a0";
          hash = "sha256-GoxtGydbFGornoq6o2pVXYzW+vUrva2F1oj7kzFM5qA=";
        };

        home.file."Pictures/Wallpapers/neon-wolf-171011c2.jpg".source = pkgs.fetchurl {
          url = "https://blossom.vlk.sh/171011c2c5f11998c2914ba12c029d79287ed28153b43292f59fce6fb1c0936e";
          hash = "sha256-FxARwsXxGZjCkUuhLAKdeSh+0oFTtDKS9Z/Ob7HAk24=";
        };

        home.username = "sebnow";
        home.homeDirectory = "/home/sebnow";
        home.stateVersion = "23.05";
        home.packages = with pkgs; [
          bottom
          calibre
          fd
          jq
          restic
        ];
        home.sessionVariables.BROWSER = "zen-browser";

        # GTK4 apps on Wayland (ghostty, gedit, ...) don't use libxkbcommon's
        # compose layer; they require an input method. fcitx5 provides compose
        # via the Wayland text-input protocol.
        i18n.inputMethod = {
          enable = true;
          type = "fcitx5";
        };

        fonts.fontconfig.enable = true;

        catppuccin.accent = "blue";

        programs.k9s.enable = true;
        programs.kitty.enable = true;
        programs.ghostty.enable = true;
        programs.ghostty.isDefault = true;
        programs.direnv = {
          enable = true;
          nix-direnv.enable = true;
        };

        targets.genericLinux = {
          enable = true;
          nixGL = {
            inherit (inputs.nixgl) packages;
            defaultWrapper = "mesa";
            installScripts = [ "mesa" ];
          };
        };
      };
  };
}
