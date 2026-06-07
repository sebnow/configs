{ ... }:
{
  # Stable nixpkgs ships nono 0.53.0 and unstable 0.57.0, but registry packs
  # we install via `mkNonoPack` declare `min_nono_version >= 0.61.0`. Bump
  # version + src + cargoDeps in-place; everything else (build inputs,
  # checkFlags, meta) is inherited from upstream.
  #
  # `cargoDeps` is overridden explicitly because nixpkgs' buildRustPackage
  # reads `args.cargoHash` (the original call's hash), not finalAttrs', so
  # `overrideAttrs` on `cargoHash` alone has no effect.
  flake.overlays.nono = _final: prev: {
    nono = prev.nono.overrideAttrs (finalAttrs: _prevAttrs: {
      version = "0.62.0";

      src = prev.fetchFromGitHub {
        owner = "always-further";
        repo = "nono";
        tag = "v${finalAttrs.version}";
        hash = "sha256-sJ8RuYOtAO5WqGJSSQnCdK4eCDszIACzrZzbmrdoeoI=";
      };

      cargoDeps = prev.rustPlatform.fetchCargoVendor {
        inherit (finalAttrs) pname version src;
        hash = "sha256-kmktowTunziarCoCOHht12DrIXyDpWgK7XZAAf4I8ok=";
      };
    });
  };
}
