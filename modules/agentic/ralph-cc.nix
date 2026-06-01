{ ... }:
{
  flake.overlays.ralph-cc = final: prev: {
    ralph-cc = prev.callPackage ../../pkgs/ralph-cc { };
  };

  flake.modules.homeManager.agentic =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.ralph-cc ];
    };
}
