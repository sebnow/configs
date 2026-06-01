{ inputs, ... }:
{
  flake.overlays.md = final: prev: {
    md = inputs.md.packages.${prev.system}.default;
  };

  flake.modules.homeManager.agentic =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.md ];
    };
}
