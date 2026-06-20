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
    nono = prev.nono.overrideAttrs (
      finalAttrs: _prevAttrs: {
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
      }
    );
  };

  flake.overlays.nono-packs = final: prev: {
    mkNonoPack = prev.callPackage ../../pkgs/nono-pack { };
    nonoPacks = {
      claude = final.mkNonoPack {
        name = "claude";
        version = "0.0.16";
        hash = "sha256-EyipJ/yHSSlSIComOHIQqtWC9tTyPm1DEQHWRjjbkXM=";
      };
      pi = final.mkNonoPack {
        name = "pi";
        version = "0.0.4";
        hash = "sha256-ktZZxjEhHBkwlHNeUUZsuB9oErc8WVmrmwvytMnDk6Q=";
      };
    };
  };

  flake.modules.homeManager.agentic =
    { pkgs, config, ... }:
    let
      goVars = config.home.sessionVariables;
      extendProfile =
        pack:
        let
          base = builtins.fromJSON (builtins.readFile "${pack}/policy.json");
        in
        base
        // {
          filesystem = base.filesystem // {
            allow = base.filesystem.allow ++ [
              "${config.xdg.cacheHome}/go-build"
              "/tmp"
              goVars.GOMODCACHE
              goVars.GOPATH
            ];
            read = [
              "$HOME/.config/git"
              "$HOME/.config/jj"
              "/etc/passwd" # git resolves the user through it
            ];
          };
        };
    in
    {
      programs.claude-code.plugins = [ pkgs.nonoPacks.claude ];

      # `nono pull` installs profiles to ~/.config/nono/profiles/. The pack's
      # `policy.json` is the profile payload; install_as in its package.json
      # determines the filename.
      home.file.".config/nono/profiles/claude.json".text = builtins.toJSON (extendProfile pkgs.nonoPacks.claude);
      home.file.".config/nono/profiles/pi.json".text = builtins.toJSON (extendProfile pkgs.nonoPacks.pi);

      # Composite profile: merges claude + pi rules so a single sandbox can
      # host both agents. `extends` resolves left-to-right; the later base
      # wins on conflicting rules. Inspect with `nono profile show claude-pi`.
      home.file.".config/nono/profiles/claude-pi.json".text = builtins.toJSON {
        meta = {
          name = "claude-pi";
          version = "1.0.0";
          description = "Composite profile: claude + pi";
        };
        extends = [
          "claude"
          "pi"
        ];
      };
    };
}
