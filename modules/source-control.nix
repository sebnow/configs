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
            merge = [
              "util"
              "exec"
              "--"
              "bash"
              "-c"
              ''
                set -euo pipefail
                src="''${1:?usage: jj merge <source> <target>}"
                tgt="''${2:?usage: jj merge <source> <target>}"
                tmpl='bookmarks.map(|b| b.name()).join(",")'
                src_name=$(jj log --no-graph -r "$src" -T "$tmpl" 2>/dev/null || true)
                tgt_name=$(jj log --no-graph -r "$tgt" -T "$tmpl" 2>/dev/null || true)
                [ -z "$src_name" ] && { echo "error: $src has no bookmark" >&2; exit 1; }
                [ -z "$tgt_name" ] && { echo "error: $tgt has no bookmark" >&2; exit 1; }
                jj new "$tgt" "$src" -m "Merge branch '$src_name' into $tgt_name"
              ''
              ""
            ];
          };
        };
      };

      programs.git = {
        enable = true;
        signing.format = "openpgp";
        ignores = [
          "*.swp"
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
