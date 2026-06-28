{ ... }:
{
  flake.overlays.lore = _final: prev: {
    lore = prev.callPackage ../pkgs/lore { };
  };

  flake.modules.homeManager.lore =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.lore ];
    };
}
