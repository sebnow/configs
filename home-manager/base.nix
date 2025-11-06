{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./neovim
  ];

  home.sessionVariables = {
    GOBIN = "${config.home.homeDirectory}/.local/bin";
    GOPATH = "${config.xdg.dataHome}/go";
    GOMODCACHE = "${config.xdg.cacheHome}/go/pkg/mod";
    GOTELEMETRY = "off";
  };

  programs.home-manager.enable = true;

  programs.bash = {
    enable = true;
    historyControl = [
      "ignorespace"
      "ignoredups"
      "erasedups"
    ];
    bashrcExtra = ''
      source <(COMPLETE=bash ${pkgs.jujutsu}/bin/jj)
    '';
  };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    history = {
      ignoreSpace = true;
      ignoreDups = true;
    };
    initContent = ''
      bindkey -v
    '';
  };

  programs.bat.enable = true;

  programs.eza = {
    enable = true;
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
      aws = { };
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
      direnv.disabled = false;
    };
  };

  programs.readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
      set keymap vi
    '';
  };

  programs.jujutsu = {
    enable = true;
    settings = {
      ui = {
        diff-formatter = "difft";
      };
      git.write-change-id-header = true;
      snapshot.auto-track = "none()";
      merge-tools = {
        difft = {
          program = "${pkgs.difftastic}/bin/difft";
          diff-args = [
            "--color=always"
            "--display=inline"
            "$left"
            "$right"
          ];
        };
        mergiraf = {
          program = "${pkgs.mergiraf}/bin/mergiraf";
          diff-args = [ ];
        };
      };
      aliases = {
        ghclone = [
          "util"
          "exec"
          "--"
          "bash"
          "-c"
          "jj git clone git@github.com:$1.git"
          ""
        ];
      };
    };
  };

  programs.git = {
    enable = true;
    ignores = [
      ".claude/"
      "*.swp"
      ".envrc"
      ".tool-versions"
      ".direnv/"
    ];
    includes = [
      { path = "config.local"; }
    ];
    settings = {
      alias = {
        st = "status --short";
        squash = "rebase -i --autosquash @{u}";
        pushf = "push --force-with-lease";
      };
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
