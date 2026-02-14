{ ... }:
{
  flake.modules.homeManager.terminals = { config, pkgs, lib, ... }:
  let
    ghosttyCfg = config.programs.ghostty;
    kittyCfg = config.programs.kitty;
  in
  {
    options.programs.ghostty.isDefault = lib.mkEnableOption "as the default terminal";
    options.programs.kitty.isDefault = lib.mkEnableOption "as the default terminal";

    config = lib.mkMerge [
      # Ghostty
      (lib.mkIf ghosttyCfg.enable {
        programs.ghostty = {
          package = if pkgs.stdenv.isDarwin then null else (config.lib.nixGL.wrap pkgs.ghostty);
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

        home.sessionVariables.TERMINAL = lib.mkIf ghosttyCfg.isDefault "ghostty";
      })
      (lib.mkIf (ghosttyCfg.enable && pkgs.stdenv.isLinux) {
        xdg.configFile."systemd/user/app-com.mitchellh.ghostty.service" = lib.mkForce {
          text =
            let
              unwrappedGhostty = pkgs.ghostty;
              wrappedGhostty = config.lib.nixGL.wrap pkgs.ghostty;
            in
            builtins.replaceStrings [ "${unwrappedGhostty}/bin/ghostty" ] [ "${wrappedGhostty}/bin/ghostty" ] (
              builtins.readFile "${unwrappedGhostty}/share/systemd/user/app-com.mitchellh.ghostty.service"
            );
        };
      })

      # Kitty
      (lib.mkIf kittyCfg.enable {
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

        programs.bash.initExtra = lib.mkIf kittyCfg.shellIntegration.enableBashIntegration ''
          [[ "$TERM" = "xterm-kitty" ]] && alias ssh='kitty +kitten ssh'
        '';

        home.sessionVariables.TERMINAL = lib.mkIf kittyCfg.isDefault "kitty";
      })

      # Alacritty
      (lib.mkIf config.programs.alacritty.enable {
        programs.alacritty = {
          settings = {
            window = {
              padding = {
                x = 5;
                y = 5;
              };
              dynamic_padding = true;
              decorations = "full";
            };
            scrolling = {
              history = 10000;
              multiplier = 3;
            };
            font = {
              normal.family = "IosevkaTerm NFM";
              size = 12;
            };
            colors = {
              primary = {
                background = "#1E1E2E"; # base
                foreground = "#CDD6F4"; # text
                dim_foreground = "#CDD6F4"; # text
                bright_foreground = "#CDD6F4"; # text
              };
              cursor = {
                text = "#1E1E2E"; # base
                cursor = "#F5E0DC"; # rosewater
              };
              vi_mode_cursor = {
                text = "#1E1E2E"; # base
                cursor = "#B4BEFE"; # lavender
              };
              search = {
                matches = {
                  foreground = "#1E1E2E"; # base
                  background = "#A6ADC8"; # subtext0
                };
                focused_match = {
                  foreground = "#1E1E2E"; # base
                  background = "#A6E3A1"; # green
                };
                footer_bar = {
                  foreground = "#1E1E2E"; # base
                  background = "#A6ADC8"; # subtext0
                };
              };
              hints = {
                start = {
                  foreground = "#1E1E2E"; # base
                  background = "#F9E2AF"; # yellow
                };
                end = {
                  foreground = "#1E1E2E"; # base
                  background = "#A6ADC8"; # subtext0
                };
              };
              selection = {
                text = "#1E1E2E"; # base
                background = "#F5E0DC"; # rosewater
              };
              normal = {
                black = "#45475A"; # surface1
                red = "#F38BA8"; # red
                green = "#A6E3A1"; # green
                yellow = "#F9E2AF"; # yellow
                blue = "#89B4FA"; # blue
                magenta = "#F5C2E7"; # pink
                cyan = "#94E2D5"; # teal
                white = "#BAC2DE"; # subtext1
              };
              bright = {
                black = "#585B70"; # surface2
                red = "#F38BA8"; # red
                green = "#A6E3A1"; # green
                yellow = "#F9E2AF"; # yellow
                blue = "#89B4FA"; # blue
                magenta = "#F5C2E7"; # pink
                cyan = "#94E2D5"; # teal
                white = "#A6ADC8"; # subtext0
              };
              dim = {
                black = "#45475A"; # surface1
                red = "#F38BA8"; # red
                green = "#A6E3A1"; # green
                yellow = "#F9E2AF"; # yellow
                blue = "#89B4FA"; # blue
                magenta = "#F5C2E7"; # pink
                cyan = "#94E2D5"; # teal
                white = "#BAC2DE"; # subtext1
              };
              indexed_colors = [
                {
                  index = 16;
                  color = "#FAB387";
                }
                {
                  index = 17;
                  color = "#F5E0DC";
                }
              ];
            };
          };
        };
      })
    ];
  };
}
