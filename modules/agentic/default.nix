{ inputs, lib, ... }:
{
  flake.overlays.pi-coding-agent = final: prev: {
    pi-coding-agent = prev.callPackage ../../pkgs/pi-coding-agent { };
  };

  flake.overlays.ralph-cc = final: prev: {
    ralph-cc = prev.callPackage ../../pkgs/ralph-cc { };
  };

  flake.overlays.zigdoc = final: prev: {
    zigdoc = prev.callPackage ../../pkgs/zigdoc { };
  };

  flake.overlays.md = final: prev: {
    md = inputs.md.packages.${prev.system}.default;
  };

  flake.overlays.mlflow-server = final: prev: {
    mlflow-server = prev.callPackage ../../pkgs/mlflow-server { };
  };

  flake.modules.homeManager.agentic =
    { pkgs, config, ... }:
    let
      piThemes = inputs.pi-coding-agent-catppuccin.packages.${pkgs.system}.default;
    in
    {
      home.packages = [
        pkgs.ast-grep
        pkgs.jq
        pkgs.md
        pkgs.mlflow-server
        pkgs.nono
        pkgs.nushell
        pkgs.pi-coding-agent
        pkgs.ralph-cc
        pkgs.tmux
        pkgs.zigdoc
      ];

      programs.claude-code = {
        enable = true;
        context = ./agents.md;
        agentsDir = ./agents;
        hooksDir = ./hooks;
        skills = ./skills;
        settings = {
          editorMode = "vim";
          feedbackSurveyRate = 0;
          fileSuggestion =
            let
              cmd = pkgs.writeShellScript "claude-file-suggestion" ''
                set -euo pipefail
                cd "''${CLAUDE_PROJECT_DIR:-$PWD}" || exit 0

                query=$(${pkgs.jq}/bin/jq -r '.query // empty')
                # Trailing `|| true` swallows non-zero exits from two benign
                # cases: head closes its pipe early (SIGPIPE 141) and fzf
                # returns 1 when the filter has no matches.
                ${pkgs.fd}/bin/fd --type f -H --no-ignore-vcs -E .git -E .jj -E node_modules --color=never \
                  | ${pkgs.fzf}/bin/fzf --filter="$query" \
                  | head -n 15 \
                  || true
              '';
            in
            {
              type = "command";
              command = "${cmd}";
            };
          statusLine =
            let
              cmd = pkgs.writeShellScript "claude-statusline" ''
                set -euo pipefail
                input=$(cat)

                model=$(printf '%s' "$input" | ${pkgs.jq}/bin/jq -r '.model.display_name // .model.id // "?"' || echo "?")
                ctx=$(printf '%s' "$input" | ${pkgs.jq}/bin/jq -r '
                  ((.context_window.current_usage // {}) | ((.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0))) as $t |
                  (.context_window.used_percentage // 0 | floor) as $p |
                  (if $t >= 1000 then (($t / 1000 | floor) | tostring) + "k" else ($t | tostring) end)
                    + "(" + ($p | tostring) + "%)"
                ' || echo "?(% ?)")
                rate_5h=$(printf '%s' "$input" | ${pkgs.jq}/bin/jq -r 'if .rate_limits.five_hour.used_percentage != null then (.rate_limits.five_hour.used_percentage | floor | tostring) + "%" else "" end' || echo "")
                rate_7d=$(printf '%s' "$input" | ${pkgs.jq}/bin/jq -r 'if .rate_limits.seven_day.used_percentage != null then (.rate_limits.seven_day.used_percentage | floor | tostring) + "%" else "" end' || echo "")

                parts=("$model" "ctx:$ctx")
                [[ -n "$rate_5h" ]] && parts+=("5h:$rate_5h")
                [[ -n "$rate_7d" ]] && parts+=("7d:$rate_7d")

                (IFS=" | "; printf '%s\n' "''${parts[*]}")
              '';
            in
            {
              type = "command";
              command = "${cmd}";
            };
          includeCoAuthoredBy = false;
          includeGitInstructions = false;
          model = "opusplan";
          showClearContextOnPlanAccept = true;
          showThinkingSummaries = true;
          disabledTools = [ "AskUserQuestion" ];
          hooks.SessionStart = [
            {
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/detect-vcs";
                }
              ];
            }
          ];
          hooks.PostToolUse = [
            {
              matcher = "Edit|Write";
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/validate-skill";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/gofmt";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/zigfmt";
                }
              ];
            }
          ];
          permissions = {
            allow = [
              "Bash(ast-grep:*)"
              "Bash(cat:*)"
              "Bash(fd:*)"
              "Bash(git commit:*)"
              "Bash(git diff:*)"
              "Bash(git log:*)"
              "Bash(git status:*)"
              "Bash(go build:*)"
              "Bash(go doc:*)"
              "Bash(go list:*)"
              "Bash(go mod:*)"
              "Bash(go run:*)"
              "Bash(go test:*)"
              "Bash(go vet:*)"
              "Bash(grep:*)"
              "Bash(jj commit:*)"
              "Bash(jj diff:*)"
              "Bash(jj edit:*)"
              "Bash(jj file list:*)"
              "Bash(jj log:*)"
              "Bash(jj new:*)"
              "Bash(jj op log:*)"
              "Bash(jj rebase:*)"
              "Bash(jj show:*)"
              "Bash(jj status:*)"
              "Bash(jq:*)"
              "Bash(ls:*)"
              "Bash(obsidian-cli:*)"
              "Bash(rg:*)"
              "Bash(tee:*)"
              "Bash(zig build:*)"
              "Bash(zigdoc:*)"
              "Glob"
              "Grep"
              "Skill(adr-writing)"
              "Skill(commit)"
              "Skill(council)"
              "Skill(jujutsu)"
              "Skill(obsidian-cli)"
              "Skill(prompt-engineering)"
              "Skill(red-green-refactor)"
              "Skill(refactor)"
              "Skill(skill-writing)"
              "Skill(systematic-debugging)"
              "Skill(tracing-knowledge-lineages)"
              "Skill(verification-before-completion)"
              "Skill(writing-clearly-and-concisely)"
            ];
            ask = [
              "Bash(git push:*)"
              "Bash(jj describe:*)"
              "Bash(jj squash:*)"
            ];
            deny = [
              "Read(./.env)"
              "Read(./.env.*)"
              "Read(./.envrc)"
            ];
          };
          # Set MLFLOW_EXPERIMENT_NAME per project in .claude/settings.json or .claude/settings.local.json
          env = {
            MLFLOW_CLAUDE_TRACING_ENABLED = "true";
            MLFLOW_TRACKING_URI = "sqlite:///${config.xdg.dataHome}/claude/mlflow.db";
          };
          hooks.Stop = [
            {
              hooks = [
                {
                  type = "command";
                  command = "mlflow autolog claude stop-hook";
                }
              ];
            }
          ];
        };
      };

      home.file.".pi/agent/themes/catppuccin-${config.catppuccin.flavor}.json".source =
        "${piThemes}/share/pi/themes/catppuccin-${config.catppuccin.flavor}.json";

      home.file.".pi/agent/settings.json".text = builtins.toJSON {
        quietStartup = true;
        editorPaddingX = 1;
        theme = "catppuccin-${config.catppuccin.flavor}";
        defaultProvider = "github-copilot";
        defaultModel = "claude-sonnet.6";
        enabledModels = [
          "github-copilot/claude-opus-4.6"
          "github-copilot/claude-sonnet-4.6"
          "github-copilot/gpt-5.3-codex"
        ];
      };

      home.file.".pi/agent/AGENTS.md".source = ./agents.md;

      home.file.".pi/agent/skills" = {
        source = ./skills;
        recursive = true;
      };

      programs.git = {
        ignores = [
          ".agents/"
          ".claude/settings.local.json"
        ];
      };
    };
}
