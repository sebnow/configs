{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./modules/home-manager/neovim
  ];

  nixpkgs.config.allowUnfree = true;

  home.username = "sebnow";
  home.homeDirectory = "/home/sebnow";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "firefox";
    TERMINAL = "kitty";
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

  programs.exa = {
    enable = true;
    enableAliases = true;
    git = true;
  };

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
      ".envrc"
      ".tool-versions"
      ".direnv/"
    ];
    includes = [
      {path = ".gitconfig.local";}
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

  #programs.kitty = {
  #  enable = true;
  #  theme = "Catppuccin-Mocha";
  #  font = {
  #    name = "Iosevka Term";
  #    size = 12;
  #  };
  #  settings = {
  #    enabled_layouts = "tall,*";
  #    scrollback_pager_history_size = 100000;

  #    tab_bar_edge = "top";
  #    tab_bar_style = "separator";
  #    tab_separator = "\" | \"";
  #  };

  #  keybindings = {
  #    "ctrl+shift+enter" = "new_window_with_cwd";
  #    "ctrl+shift+z" = "toggle_layout stack";
  #  };
  #};

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
}
