{
  description = "Home Manager configuration of sebnow";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    home-manager,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;

      config.allowUnfree = true;
    };
  in {
    formatter.${system} = pkgs.alejandra;

    homeConfigurations."sebnow" = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules = [./home.nix];
    };
  };
}
