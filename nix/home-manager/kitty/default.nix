{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.programs.kitty;
in {
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.kitty = {
        theme = "Catppuccin-Mocha";
        font = {
          name = "IosevkaTerm NFM";
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

      home.sessionVariables.TERMINAL = "kitty";

      # FIXME: Remove once GLX issues are solved on standalone
      # installations
      # https://github.com/NixOS/nixpkgs/issues/80936
      #
      # Kitty is not able to create a GLFW window.
      #
      # This works around the issue by using the executable provided by
      # the system. That means the only purpose of this module is to
      # provide the configuration.
      home.shellAliases.kitty = "/usr/bin/kitty";
      xdg.dataFile."applications/kitty.desktop" = {
        text = ''
          [Desktop Entry]
          Version=1.0
          Type=Application
          Name=kitty
          GenericName=Terminal emulator
          Comment=Fast, feature-rich, GPU based terminal
          TryExec=/usr/bin/kitty
          Exec=/usr/bin/kitty
          Icon=kitty
          Categories=System;TerminalEmulator;
        '';
      };
    })
  ];
}
