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
          inputs.ghostty.overlays.default
          (final: prev: {
            camunda-modeler = prev.callPackage ../pkgs/camunda-modeler { };
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
                  nixGL = {
                    inherit (inputs.nixgl) packages;
                    defaultWrapper = "mesa";
                    installScripts = [ "mesa" ];
                  };

                  targets.genericLinux.enable = true;
                }
              )
            ];
          };
    };
}
