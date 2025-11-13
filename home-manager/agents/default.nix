{ ... }:
{
  programs.claude-code = {
    enable = true;
    memory.source = ./agents.md;
    agents = {
      architect = ./agent-architect.md;
      code-reviewer = ./agent-code-reviewer.md;
      coder = ./agent-coder.md;
      debugger = ./agent-debugger.md;
      tester = ./agent-tester.md;
    };
    settings = {
      permissions = {
        allow = [
          "Bash(cat:*)"
          "Bash(fd:*)"
          "Bash(git diff:*)"
          "Bash(git log:*)"
          "Bash(git status:*)"
          "Bash(go build:*)"
          "Bash(go doc:*)"
          "Bash(go list:*)"
          "Bash(go run:*)"
          "Bash(go test:*)"
          "Bash(grep:*)"
          "Bash(jj diff:*)"
          "Bash(jj log:*)"
          "Bash(jj show:*)"
          "Bash(jj status:*)"
          "Bash(jq:*)"
          "Bash(ls:*)"
          "Bash(rg:*)"
          "Bash(tee:*)"
          "Bash(zig build:*)"
          "Glob"
          "Grep"
        ];
        ask = [
          "Bash(ast-grep)"
          "Bash(git commit:*)"
          "Bash(git push:*)"
          "Bash(go mod:*)"
          "Bash(jj commit:*)"
          "Bash(jj describe:*)"
          "Bash(jj edit:*)"
          "Bash(jj new:*)"
          "Bash(jj rebase:*)"
          "Bash(jj squash:*)"
          "WebFetch"
        ];
        deny = [
          "Read(./.env)"
          "Read(./.env.*)"
          "Read(./.envrc)"
        ];
      };
    };
  };

  home.file = builtins.listToAttrs (
    builtins.map
      (s: {
        name = ".claude/skills/${s}/SKILL.md";
        value = {
          source = ./. + "/skill-${s}.md";
        };
      })
      [
        "source-control-hygiene"
        "systematic-debugging"
        "verification-before-completion"
        "writing-clearly-and-concisely"
      ]
  );
}
