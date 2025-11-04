{ pkgs, ... }:
let
  instructions = builtins.readFile ./agents.md;
  agents = {
    debugger = builtins.readFile ./agent-debugger.md;
    code-reviewer = builtins.readFile ./agent-code-reviewer.md;
    qa = builtins.readFile ./agent-qa.md;
  };
in
{
  home.packages = [
    pkgs.claude-code
  ];

  programs.claude-code = {
    enable = true;
    memory.text = instructions;
    agents = agents;
    settings = {
      permissions = {
        permissions = {
          allow = [
            "Bash(fd)"
            "Bash(git diff:*)"
            "Bash(git log:*)"
            "Bash(git status:*)"
            "Bash(go build:*)"
            "Bash(go list:*)"
            "Bash(jj diff:*)"
            "Bash(jj log:*)"
            "Bash(jj status:*)"
            "Bash(jq)"
            "Bash(ls)"
            "Bash(rg)"
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
  };

}
