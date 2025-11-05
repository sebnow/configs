{ ... }:
let
  instructions = builtins.readFile ./agents.md;
  agents = {
    debugger = builtins.readFile ./agent-debugger.md;
    code-reviewer = builtins.readFile ./agent-code-reviewer.md;
    tester = builtins.readFile ./agent-tester.md;
  };
in
{
  programs.claude-code = {
    enable = true;
    memory.text = instructions;
    agents = agents;
    settings = {
      permissions = {
        allow = [
          "Bash(cat:*)"
          "Bash(fd)"
          "Bash(fd:*)"
          "Bash(git diff:*)"
          "Bash(git log:*)"
          "Bash(git status:*)"
          "Bash(go build:*)"
          "Bash(go doc:*)"
          "Bash(go list:*)"
          "Bash(go run:*)"
          "Bash(go test:*)"
          "Bash(jj diff:*)"
          "Bash(jj log:*)"
          "Bash(jj show:*)"
          "Bash(jj status:*)"
          "Bash(jq)"
          "Bash(ls)"
          "Bash(ls:*)"
          "Bash(rg)"
          "Bash(rg:*)"
          "Bash(zig build:*)"
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
}
