{ ... }:
{
  flake.overlays.zigdoc = final: prev: {
    zigdoc = prev.callPackage ../../pkgs/zigdoc { };
  };

  flake.modules.homeManager.agentic =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.zigdoc ];
    };
}
