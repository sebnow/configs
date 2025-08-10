{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.programs.ghostty;
in
{
  options.programs.ghostty = {
    isDefault = lib.mkEnableOption "as the default terminal";
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.ghostty = {
        package = if pkgs.stdenv.isDarwin then null else pkgs.ghostty;
        settings = {
          font-size = if pkgs.stdenv.isDarwin then 14 else 12;
          font-family = "IosevkaTerm NF";
          theme = "catppuccin-${config.catppuccin.flavor}";
          keybind = pkgs.lib.optionals pkgs.stdenv.isLinux [
            "ctrl+shift+enter=new_split:right"
            "ctrl+shift+d=new_split:down"
            "ctrl+shift+z=toggle_split_zoom"
            "ctrl+shift+[=goto_split:previous"
            "ctrl+shift+]=goto_split:next"
            "ctrl+shift+h=goto_split:left"
            "ctrl+shift+j=goto_split:bottom"
            "ctrl+shift+k=goto_split:top"
            "ctrl+shift+l=goto_split:right"
          ];
        };
      };

      home.sessionVariables.TERMINAL = lib.mkIf cfg.isDefault "ghostty";
    })

    (lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) (
      let
        ghostty = "${pkgs.nixgl.nixGLMesa}/bin/nixGLMesa ${pkgs.ghostty}/bin/ghostty";
      in
      {
        # FIXME: Remove once GLX issues are solved on standalone
        # installations
        # https://github.com/NixOS/nixpkgs/issues/80936
        #
        # Failed to create EGL display
        home.shellAliases.ghostty = ghostty;
        xdg.dataFile."applications/ghostty.desktop".text = ''
          [Desktop Entry]
          Name=Ghostty
          Type=Application
          Comment=A terminal emulator
          TryExec=${pkgs.ghostty}/bin/ghostty
          Exec=${ghostty}
          Icon=com.mitchellh.ghostty
          Categories=System;TerminalEmulator;
          Keywords=terminal;tty;pty;
          StartupNotify=true
          Terminal=false
          Actions=new-window;
          X-GNOME-UsesNotifications=true
          X-TerminalArgExec=-e
          X-TerminalArgTitle=--title=
          X-TerminalArgAppId=--class=
          X-TerminalArgDir=--working-directory=
          X-TerminalArgHold=--wait-after-command

          [Desktop Action new-window]
          TryExec=${pkgs.ghostty}/bin/ghostty
          Exec=${ghostty}
        '';
      }
    ))
  ];
}
