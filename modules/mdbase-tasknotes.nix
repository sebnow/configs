{ ... }:
{
  flake.overlays.mdbase-tasknotes = _final: prev: {
    mdbase-tasknotes = prev.callPackage ../pkgs/mdbase-tasknotes { };
  };

  flake.modules.homeManager.mdbase-tasknotes =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.mdbase-tasknotes ];
    };
}
