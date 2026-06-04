{ ... }:
{
  flake.modules.homeManager.agentic =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    let
      catppuccinFlavor = config.catppuccin.flavor;
      catppuccinAccent = config.catppuccin.accent;
      catppuccinPalette =
        (lib.importJSON "${config.catppuccin.sources.palette}/palette.json").${catppuccinFlavor}.colors;
      hex = name: catppuccinPalette.${name}.hex;
      claudeThemeName = "catppuccin-${catppuccinFlavor}-${catppuccinAccent}";
    in
    {
      home.packages = [
        pkgs.ast-grep
        pkgs.jq
        pkgs.nono
        pkgs.nushell
        pkgs.tmux
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
          tui = "fullscreen";
          theme = claudeThemeName;
          includeCoAuthoredBy = false;
          includeGitInstructions = false;
          model = "opusplan";
          skillOverrides = {
            claude-api = "off";
            code-review = "off";
            fewer-permission-prompts = "off";
            init = "off";
            keybindings-help = "off";
            loop = "off";
            review = "off";
            run = "off";
            schedule = "off";
            security-review = "off";
            simplify = "off";
            update-config = "off";
            verify = "off";
          };
          showClearContextOnPlanAccept = true;
          showThinkingSummaries = true;
          hooks.SessionStart = [
            {
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/detect-vcs";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/inject-agents-md";
                }
              ];
            }
          ];
          hooks.PreToolUse = [
            {
              matcher = "Bash";
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/vcs-guard";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/block-test-output-filtering";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/obsidian-cli-guard";
                }
              ];
            }
            # Blocks emoji (and future rules) in new markdown content.
            # See .agents/prd-markdown.md
            {
              matcher = "Edit|Write";
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/mdlint";
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
              "AskUserQuestion"
              "Read(./.env)"
              "Read(./.env.*)"
              "Read(./.envrc)"
            ];
          };
          env = {
            ENABLE_CLAUDEAI_MCP_SERVERS = false;
          };
          hooks.Stop = [
            {
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/vcs-working-copy-guard";
                }
              ];
            }
          ];
        };
      };

      home.file.".claude/themes/${claudeThemeName}.json".text = builtins.toJSON {
        name = claudeThemeName;
        base = if catppuccinFlavor == "latte" then "light" else "dark";
        overrides = {
          claude = hex catppuccinAccent;
          primary = hex catppuccinAccent;
          accent = hex catppuccinAccent;
          secondary = hex "mauve";
          tertiary = hex "teal";
          background = hex "base";
          foreground = hex "text";
          text = hex "text";
          muted = hex "overlay1";
          border = hex "surface1";
          surface = hex "surface0";
          overlay = hex "overlay0";
          highlight = hex "surface1";
          selection = hex "surface2";
          cursor = hex "rosewater";
          error = hex "red";
          warning = hex "yellow";
          success = hex "green";
          info = hex "sapphire";
        };
      };

      programs.git = {
        ignores = [
          ".agents/"
          ".claude/CLAUDE.local.md"
          ".claude/settings.local.json"
        ];
      };
    };
}
