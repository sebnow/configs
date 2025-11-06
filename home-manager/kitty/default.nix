{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.programs.kitty;
in
{
  options.programs.kitty = {
    isDefault = lib.mkEnableOption "as the default terminal";
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.kitty = {
        package = config.lib.nixGL.wrap pkgs.kitty;
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
    })
  ];
}
