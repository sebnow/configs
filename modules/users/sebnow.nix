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
