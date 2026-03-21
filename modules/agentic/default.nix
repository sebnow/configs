{ inputs, ... }:
{
  flake.overlays.skills-ref = final: prev: {
    skills-ref = prev.python3Packages.callPackage ../../pkgs/skills-ref { };
  };

  flake.overlays.pi-coding-agent = final: prev: {
    pi-coding-agent = prev.callPackage ../../pkgs/pi-coding-agent { };
  };

  flake.overlays.zigdoc = final: prev: {
    zigdoc = prev.callPackage ../../pkgs/zigdoc { };
  };

  flake.overlays.md = final: prev: {
    md = inputs.md.packages.${prev.system}.default;
  };

  flake.modules.homeManager.agentic =
    { pkgs, config, ... }:
    let
      piThemes = inputs.pi-coding-agent-catppuccin.packages.${pkgs.system}.default;
      vaultDebriefProcess = pkgs.writeShellScriptBin "vault-debrief-process" ''
        set -euo pipefail

        queue_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/agents/agent-vault-debrief-queue"

        if [[ ! -d "$queue_dir" ]]; then
          exit 0
        fi

        for entry in "$queue_dir"/*; do
          session_id=$(${pkgs.coreutils}/bin/basename "$entry")
          [[ "$session_id" = "*" ]] && break

          transcript=$(${pkgs.findutils}/bin/find "$HOME/.claude/projects" -name "$session_id.jsonl" 2>/dev/null | head -1)
          if [[ -z "$transcript" ]]; then
            rm -f "$entry"
            continue
          fi

          # Skip sessions with no meaningful content (e.g. cancelled /resume)
          msg_count=$(${pkgs.jq}/bin/jq -c 'select(.type == "user" or .type == "assistant")' "$transcript" | wc -l)
          if [[ "$msg_count" -lt 2 ]]; then
            rm -f "$entry"
            continue
          fi

          # Skip debrief sessions to avoid debriefing our own runs
          first_user_msg=$(${pkgs.jq}/bin/jq -n -r 'first(inputs | select(.type == "user")) | .message.content | if type == "string" then . else .[0].text // "" end' "$transcript")
          if [[ "$first_user_msg" == *"Run /agent-vault debrief for session"* ]]; then
            rm -f "$entry"
            continue
          fi

          project_dir=$(${pkgs.coreutils}/bin/dirname "$transcript")
          project_name=$(${pkgs.coreutils}/bin/basename "$project_dir" | rev | cut -d- -f1 | rev)

          if echo "Run /agent-vault debrief for session $session_id.
        The session transcript is at: $transcript
        The project name is: $project_name
        This is a non-interactive background process. Do not ask questions. If the session has no meaningful content to debrief, silently skip it." | ${pkgs.claude-code}/bin/claude --print \
            --allowedTools 'Edit,Glob,Grep,Read,Write,Skill(agent-vault)'; then
            rm -f "$entry"
          else
            echo "Failed to debrief session: $session_id" >&2
          fi
        done
      '';

      piWrapped = pkgs.writeShellScriptBin "bwrap-pi" ''
        set -euo pipefail
        (exec ${pkgs.bubblewrap}/bin/bwrap \
          --dir /tmp \
          --dir /var \
          --bind "$HOME/.pi" "$HOME/.pi" \
          --bind "$PWD" "$PWD" \
          --ro-bind "$HOME/.nix-profile" "$HOME/.nix-profile" \
          --ro-bind "$HOME/.config/git" "$HOME/.config/git" \
          --ro-bind /nix/store /nix/store \
          --ro-bind /nix/var/nix/profiles/default /nix/var/nix/profiles/default \
          --ro-bind /etc/resolv.conf /etc/resolv.conf \
          --ro-bind /etc/ssl /etc/ssl \
          --ro-bind /etc/ca-certificates /etc/ca-certificates \
          --ro-bind /usr /usr \
          --symlink usr/lib /lib \
          --symlink usr/lib64 /lib64 \
          --symlink usr/bin /bin \
          --symlink usr/sbin /sbin \
          --proc /proc \
          --dev /dev \
          --unshare-all \
          --share-net \
          --unshare-pid \
          --die-with-parent \
          --new-session \
          --dir /run/user/$(${pkgs.coreutils}/bin/id -u) \
          --setenv XDG_RUNTIME_DIR "/run/user/$(${pkgs.coreutils}/bin/id -u)" \
          ''${SSH_AUTH_SOCK:+--bind "$SSH_AUTH_SOCK" "/run/user/$(${pkgs.coreutils}/bin/id -u)/ssh-agent.sock"} \
          ''${SSH_AUTH_SOCK:+--setenv SSH_AUTH_SOCK "/run/user/$(${pkgs.coreutils}/bin/id -u)/ssh-agent.sock"} \
          --file 11 /etc/passwd \
          --file 12 /etc/group \
          ${pkgs.pi-coding-agent}/bin/pi) \
        11< <(getent passwd $UID 65534) \
        12< <(getent group $(${pkgs.coreutils}/bin/id -g) 65534)
      '';
    in
    {
      home.packages = [
        piWrapped
        pkgs.ast-grep
        pkgs.jq
        pkgs.md
        pkgs.nushell
        pkgs.pi-coding-agent
        pkgs.skills-ref
        pkgs.tmux
        pkgs.zigdoc
      ];

      programs.claude-code = {
        enable = true;
        memory.source = ./agents.md;
        agentsDir = ./agents;
        hooksDir = ./hooks;
        skillsDir = ./skills;
        settings = {
          includeCoAuthoredBy = false;
          hooks.SessionStart = [
            {
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/detect-vcs";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/vault-context";
                }
              ];
            }
          ];
          hooks.SessionEnd = [
            {
              hooks = [
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/vault-debrief";
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
                  command = "bash \"$HOME/.claude/hooks/validate-skill\"";
                }
                {
                  type = "command";
                  command = "$HOME/.claude/hooks/gofmt";
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
              "Bash(rg:*)"
              "Bash(tee:*)"
              "Bash(zig build:*)"
              "Bash(zigdoc:*)"
              "Glob"
              "Grep"
              "Skill(adr-writing)"
              "Skill(commit)"
              "Skill(jujutsu)"
              "Skill(prompt-engineering)"
              "Skill(red-green-refactor)"
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

      systemd.user.services.vault-debrief = {
        Unit.Description = "Process vault debrief queue";
        Service = {
          Type = "oneshot";
          ExecStart = "${vaultDebriefProcess}/bin/vault-debrief-process";
        };
      };

      systemd.user.timers.vault-debrief = {
        Unit.Description = "Process vault debrief queue periodically";
        Timer = {
          OnCalendar = "hourly";
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };

      programs.git = {
        ignores = [
          ".agents/"
          ".claude/"
        ];
      };
    };
}
