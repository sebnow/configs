{
  self,
  inputs,
  ...
}: {
  perSystem = {
    config,
    pkgs,
    ...
  }: {
    legacyPackages.homeConfigurations."sebnow" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [./sebnow.nix];
    };
  };
}
