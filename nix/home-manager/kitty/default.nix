{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.programs.kitty;
  kitty = "${pkgs.nixgl.nixGLMesa}/bin/nixGLMesa ${pkgs.kitty}/bin/kitty";
in {
  options.programs.kitty = {
    isDefault = lib.mkEnableOption "as the default terminal";
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.kitty = {
        font = {
          name = "IosevkaTerm NF";
          size = 12;
        };
        settings = {
          enabled_layouts = "tall,*";
          scrollback_pager_history_size = 100000;

          tab_bar_edge = "top";
          tab_bar_style = "separator";
          tab_separator = "\" | \"";
          linux_display_server = "x11";
        };

        keybindings = {
          "ctrl+shift+enter" = "new_window_with_cwd";
          "ctrl+shift+z" = "toggle_layout stack";
        };
      };

      programs.bash.initExtra = lib.mkIf cfg.shellIntegration.enableBashIntegration ''
        [[ "$TERM" = "xterm-kitty" ]] && alias ssh='kitty +kitten ssh'
      '';

      home.sessionVariables.TERMINAL = lib.mkIf cfg.isDefault "kitty";

      # FIXME: Remove once GLX issues are solved on standalone
      # installations
      # https://github.com/NixOS/nixpkgs/issues/80936
      #
      # Kitty is not able to create a GLFW window.
      home.shellAliases.kitty = kitty;
      xdg.dataFile."applications/kitty.desktop" = {
        text = ''
          [Desktop Entry]
          Version=1.0
          Type=Application
          Name=kitty
          GenericName=Terminal emulator
          Comment=Fast, feature-rich, GPU based terminal
          TryExec=${pkgs.kitty}/bin/kitty
          Exec=${kitty}
          Icon=${pkgs.kitty}/share/icons/hicolor/scalable/apps/kitty.svg
          Categories=System;TerminalEmulator;
        '';
      };
    })
  ];
}
