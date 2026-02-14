{ ... }:
{
  flake.modules.homeManager.tmux = { ... }: {
    programs.tmux = {
      enable = true;
      shortcut = "\\";
      mouse = true;
      keyMode = "vi";
      customPaneNavigationAndResize = true;
      reverseSplit = true;
      extraConfig = ''
        bind r source-file ~/.config/tmux/tmux.conf
        bind '"' choose-tree -w

        # Status line
        set -g status-position top
        set -g status-style fg=white,bg=default,none
        set -g status-left " "
        set -g status-justify left
        set -g status-right " "
        set -g message-style fg=cyan,bg=brightblack
        set -g message-command-style fg=cyan,bg=brightblack
        set -g pane-border-style bg=black,fg=black
        set -g pane-active-border-style bg=black,fg=brightblack
        set -g display-panes-colour black
        set -g display-panes-active-colour brightblack

        setw -g window-status-format '  #I:#W  '
        setw -g window-status-current-format '  #W  '
        setw -g window-status-style default
        setw -g window-status-current-style fg=default,bg=brightblack,bold
      '';
    };
  };
}
