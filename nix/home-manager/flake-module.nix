{
  self,
  inputs,
  ...
}: {
  perSystem = {
    system,
    config,
    pkgs,
    ...
  }: let
    pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [inputs.neovim.overlay inputs.nixgl.overlay];
    };
  in {
    legacyPackages.homeConfigurations."sebnow" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [./sebnow.nix];
    };
  };
}
