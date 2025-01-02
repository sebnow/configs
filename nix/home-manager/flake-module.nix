{inputs, ...}: {
  perSystem = {system, ...}: let
    pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        inputs.neovim.overlays.default
        inputs.nixgl.overlay
        (final: prev: {
          camunda-modeler = prev.callPackage ../pkgs/camunda-modeler {};
        })
      ];
    };
  in {
    legacyPackages.homeConfigurations."sebnow@stribog" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        inputs.catppuccin.homeManagerModules.catppuccin
        ./sebnow.nix
        ({...}: {
          targets.genericLinux.enable = true;
        })
      ];
    };
  };
}
