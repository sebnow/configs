{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.neovim.overlays.default
          inputs.nixgl.overlays.default
          (final: prev: {
            camunda-modeler = prev.callPackage ../pkgs/camunda-modeler { };
            skills-ref = prev.python3Packages.callPackage ../pkgs/skills-ref { };
          })
        ];
      };
    in
    {
      legacyPackages.homeConfigurations."sebnow@stribog" =
        inputs.home-manager.lib.homeManagerConfiguration
          {
            inherit pkgs;
            extraSpecialArgs = {
              inherit (inputs) nixgl;
            };
            modules = [
              inputs.catppuccin.homeModules.catppuccin
              ./sebnow.nix
              (
                { ... }:
                {
                  targets.genericLinux = {
                    enable = true;
                    nixGL = {
                      inherit (inputs.nixgl) packages;
                      defaultWrapper = "mesa";
                      installScripts = [ "mesa" ];
                    };
                  };
                }
              )
            ];
          };
    };
}
