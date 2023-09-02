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
        };

        keybindings = {
          "ctrl+shift+enter" = "new_window_with_cwd";
          "ctrl+shift+z" = "toggle_layout stack";
        };
      };

      home.sessionVariables.TERMINAL = "kitty";
    })
  ];
}
