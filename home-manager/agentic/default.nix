{ ... }:
{
  programs.claude-code = {
    enable = true;
    memory.source = ./agents.md;
    agentsDir = ./agents;
    settings = {
      includeCoAuthoredBy = false;
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
          "Skill(adr-writing)"
          "Skill(prompt-engineering)"
          "Skill(skill-writing)"
          "Skill(source-control-hygiene)"
          "Skill(systematic-debugging)"
          "Skill(tracing-knowledge-lineages)"
          "Skill(verification-before-completion)"
          "Skill(writing-clearly-and-concisely)"
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

  home.file.".claude/skills" = {
    source = ./skills;
    recursive = true;
  };
}
