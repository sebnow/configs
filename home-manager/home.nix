{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./modules/home-manager/alacritty
    ./modules/home-manager/fonts
    ./modules/home-manager/gnome
    ./modules/home-manager/k9s
    ./modules/home-manager/kitty
    ./modules/home-manager/neovim
  ];

  nixpkgs.config.allowUnfree = true;

  home.username = "sebnow";
  home.homeDirectory = "/home/sebnow";
  home.packages = with pkgs; [
    bottom
    fd
    jq
    restic
  ];

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.sessionVariables = {
    BROWSER = "firefox";
  };

  fonts.fontconfig.enable = true;

  programs.bash = {
    enable = true;
    historyControl = ["ignorespace" "ignoredups" "erasedups"];
    bashrcExtra = ''
      [[ "$TERM" = "xterm-kitty" ]] && alias ssh='kitty +kitten ssh'
    '';
  };
  # TODO: Direnv

  programs.bat = {
    enable = true;
    config.theme = "Catppuccin-mocha";
    themes = {
      Catppuccin-mocha = builtins.readFile (pkgs.fetchFromGitHub
        {
          owner = "catppuccin";
          repo = "bat";
          rev = "ba4d16880d63e656acced2b7d4e034e4a93f74b1";
          sha256 = "sha256-6WVKQErGdaqb++oaXnY3i6/GuH2FhTgK0v4TN4Y0Wbw=";
        }
        + "/Catppuccin-mocha.tmTheme");
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
    git = true;
  };

  programs.firefox.enable = true;

  programs.fzf = {
    enable = true;
    defaultCommand = "rg --files --hidden --follow --glob \"!.git/*\"";
  };

  programs.git = {
    enable = true;
    aliases = {
      st = "status --short";
      squash = "rebase -i --autosquash @{u}";
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
    delta = {
      enable = true;
      options = {
        line-numbers = true;
        syntax-theme = "Catppuccin-mocha";
      };
    };
    # TODO
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.k9s.enable = true;
  programs.kitty.enable = true;

  programs.readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
      set keymap vi
    '';
  };

  #programs.ripgrep.enable = true;

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
