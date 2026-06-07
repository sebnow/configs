{ inputs, ... }:
{
  flake.overlays.pi-coding-agent = final: prev: {
    pi-coding-agent = prev.callPackage ../../pkgs/pi-coding-agent { };
  };

  flake.modules.homeManager.agentic =
    { pkgs, config, ... }:
    let
      piThemes = inputs.pi-coding-agent-catppuccin.packages.${pkgs.system}.default;
    in
    {
      home.packages = [ pkgs.pi-coding-agent ];

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
        packages = [
          { source = "${pkgs.nonoPacks.pi}"; }
        ];
      };

      home.file.".pi/agent/AGENTS.md".source = ./agents.md;

      home.file.".pi/agent/skills" = {
        source = ./skills;
        recursive = true;
      };
    };
}
