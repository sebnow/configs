{ ... }:
{
  flake.modules.homeManager.shell =
    { pkgs, ... }:
    {
      programs.home-manager.enable = true;

      programs.bash = {
        enable = true;
        historyControl = [
          "ignorespace"
          "ignoredups"
          "erasedups"
        ];
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
        defaultOptions = [
          "--height 30%"
          "--min-height 5"
        ];
        fileWidgetOptions = [ "--preview 'bat -p --color=always {}'" ];
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
    };
}
