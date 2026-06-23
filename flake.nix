{
  description = "Home Manager configuration of sebnow";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pi-coding-agent-catppuccin = {
      url = "github:otahontas/pi-coding-agent-catppuccin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    noctalia = {
      url = "github:noctalia-dev/noctalia/legacy-v4";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin-niri = {
      url = "github:ninetailedtori/catppuccin-niri";
      flake = false;
    };
    import-tree.url = "github:vic/import-tree";
    md = {
      url = "github:sebnow/md";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake
      {
        inherit inputs;
      }
      {
        systems = [ "x86_64-linux" ];

        imports = [
          (inputs.import-tree ./modules)
        ];

        perSystem =
          { pkgs, ... }:
          {
            formatter = pkgs.nixfmt-tree;
          };
      };
}
