{ inputs, ... }:
{
  flake.modules.homeManager.catppuccin =
    { pkgs, ... }:
    let
      catppuccin-process-compose = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "process-compose";
        rev = "4db4b805a1bfe6b48fbf3b993b29226c515151d8";
        sha256 = "sha256-bxkxdhKpZYIcPjIGX/aXhl9jKXPNREmaT1eer7R/etk=";
      };
    in
    {
      imports = [ inputs.catppuccin.homeModules.catppuccin ];

      catppuccin.enable = true;

      # detect_integrations requires git at load time (via vim.pack),
      # which isn't available in the Nix build sandbox.
      catppuccin.sources.nvim = pkgs.vimPlugins.catppuccin-nvim.overrideAttrs (old: {
        nvimSkipModule =
          (old.nvimSkipModule or [ ])
          ++ (old.nvimSkipModules or [ ])
          ++ [ "catppuccin.lib.detect_integrations" ];
      });

      xdg.configFile."process-compose/theme.yaml".source =
        catppuccin-process-compose + "/themes/catppuccin-mocha.yaml";
    };
}
