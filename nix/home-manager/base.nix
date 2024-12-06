{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./neovim
  ];

  programs.home-manager.enable = true;

  programs.bash = {
    enable = true;
    historyControl = ["ignorespace" "ignoredups" "erasedups"];
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    history = {
      ignoreSpace = true;
      ignoreDups = true;
    };
    initExtra = ''
      bindkey -v
    '';
  };

  programs.bat = {
    enable = true;
    config.theme = "Catppuccin-mocha";
    themes = {
      Catppuccin-mocha = {
        src = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "bat";
          rev = "ba4d16880d63e656acced2b7d4e034e4a93f74b1";
          sha256 = "sha256-6WVKQErGdaqb++oaXnY3i6/GuH2FhTgK0v4TN4Y0Wbw=";
        };
        file = "Catppuccin-mocha.tmTheme";
      };
    };
  };

  programs.eza = {
    enable = true;
    enableAliases = true;
    git = true;
  };

  programs.fzf = {
    enable = true;
    defaultCommand = "rg --files --hidden --follow --glob \"!.git/*\"";
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      add_newline = false;
      aws = {};
      cmd_duration.style = "dimmed yellow";
      directory = {
        style = "fg:green";
        truncate_to_repo = false;
      };
      golang.disabled = true;
      nodejs.disabled = true;
      package.disabled = true;
      python.disabled = true;
      ruby.disabled = true;
      rust.disabled = true;
    };
  };

  programs.readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
      set keymap vi
    '';
  };

  programs.git = {
    enable = true;
    aliases = {
      st = "status --short";
      squash = "rebase -i --autosquash @{u}";
      pushf = "push --force-with-lease";
    };
    ignores = [
      "*.swp"
      ".envrc"
      ".tool-versions"
      ".direnv/"
    ];
    includes = [
      {path = "config.local";}
    ];
    extraConfig = {
      apply.whitespace = "fix";
      rerere.enable = true;
      branch = {
        master = {
          mergeoptions = "--no-ff";
          rebase = true;
        };
        develop = {
          mergeoptions = "--no-ff";
        };
      };
      push.default = "simple";
      log.decorate = "short";
    };
  };

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
}
