{ ... }:
{
  flake.modules.homeManager.source-control =
    { config, pkgs, ... }:
    {
      programs.bash.bashrcExtra = ''
        source <(COMPLETE=bash ${pkgs.jujutsu}/bin/jj)
      '';

      programs.delta = {
        enable = true;
        enableGitIntegration = true;
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
          "*.swp"
          ".claude/"
          ".direnv/"
          ".envrc"
          ".private/"
          ".tool-versions"
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
    };
}
