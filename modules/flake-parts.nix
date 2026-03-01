{ lib, inputs, ... }:
{
  imports = [ inputs.flake-parts.flakeModules.modules ];
  config.flake.modules = lib.mkDefault { };
}
