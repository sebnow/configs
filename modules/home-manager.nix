{
  lib,
  config,
  inputs,
  ...
}:
{
  options.configurations.homeManager = lib.mkOption {
    type = lib.types.lazyAttrsOf (
      lib.types.submodule {
        options = {
          system = lib.mkOption { type = lib.types.str; };
          module = lib.mkOption { type = lib.types.deferredModule; };
        };
      }
    );
    default = { };
  };

  config.perSystem =
    { system, ... }:
    {
      legacyPackages.homeConfigurations =
        let
          forThisSystem = lib.filterAttrs (_: cfg: cfg.system == system) config.configurations.homeManager;
        in
        lib.mapAttrs (
          _:
          { module, system, ... }:
          inputs.home-manager.lib.homeManagerConfiguration {
            pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = builtins.attrValues config.flake.overlays;
            };
            modules = [ module ];
          }
        ) forThisSystem;
    };
}
