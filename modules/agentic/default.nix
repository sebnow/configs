{ inputs, ... }:
{
  flake.overlays.skills-ref = final: prev: {
    skills-ref = prev.python3Packages.callPackage ../../pkgs/skills-ref { };
  };

  flake.overlays.pi-coding-agent = final: prev: {
    pi-coding-agent = prev.callPackage ../../pkgs/pi-coding-agent { };
  };

  flake.overlays.md = final: prev: {
    md = inputs.md.packages.${prev.system}.default;
  };

  flake.modules.homeManager.agentic =
    { pkgs, config, ... }:
    let
      piThemes = inputs.pi-coding-agent-catppuccin.packages.${pkgs.system}.default;
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
      ];

      programs.claude-code = {
        enable = true;
        memory.source = ./agents.md;
        agentsDir = ./agents;
        hooksDir = ./hooks;
        skillsDir = ./skills;
        settings = {
          includeCoAuthoredBy = false;
          hooks.PostToolUse = [
            {
              matcher = "Edit|Write";
              hooks = [
                {
                  type = "command";
                  command = "bash \"$HOME/.claude/hooks/validate-skill\"";
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
        defaultModel = "claude-opus-4.6";
        enabledModels = [
          "github-copilot/claude-opus-4.6"
          "github-copilot/claude-sonnet-4.5"
          "github-copilot/gpt-5.2-codex"
          "github-copilot/gpt-5.2"
          "github-copilot/gemini-3-pro-preview"
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
          ".claude/"
        ];
      };
    };
}
