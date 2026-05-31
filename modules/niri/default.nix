{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      # config.kdl uses `include optional=true "theme.kdl"`, so validation
      # passes without the theme file present in the source tree.
      checks.niri-config = pkgs.runCommand "niri-config-valid" { } ''
        ${pkgs.niri}/bin/niri validate -c ${./config.kdl}
        touch $out
      '';
    };

  # niri binary is installed via pacman; this module manages only user-scoped
  # configuration and tooling. niri-flake is intentionally excluded to avoid
  # its gnome-keyring forced-enable, which conflicts with KeePassXC.
  flake.modules.homeManager.niri =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config.catppuccin) flavor accent;
      # Upstream theme files include `# Theme: ...` lines that aren't valid
      # KDL. Strip them before deploying.
      themeFile = pkgs.runCommand "catppuccin-niri-${flavor}-${accent}.kdl" { } ''
        sed -e '/^[[:space:]]*#/d' -e '/empty-workspace-above-first/d' \
          ${inputs.catppuccin-niri}/themes/${flavor}/catppuccin-${flavor}-${accent}.kdl \
          > $out
      '';
      palette = (lib.importJSON "${config.catppuccin.sources.palette}/palette.json").${flavor}.colors;
      hex = name: palette.${name}.hex;
      noctaliaColors = {
        mError = hex "red";
        mHover = hex "teal";
        mOnError = hex "crust";
        mOnHover = hex "crust";
        mOnPrimary = hex "crust";
        mOnSecondary = hex "crust";
        mOnSurface = hex "text";
        mOnSurfaceVariant = hex "subtext1";
        mOnTertiary = hex "crust";
        mOutline = hex "surface2";
        mPrimary = hex accent;
        mSecondary = hex "peach";
        mShadow = hex "crust";
        mSurface = hex "base";
        mSurfaceVariant = hex "surface0";
        mTertiary = hex "teal";
      };
    in
    {
      imports = [ inputs.noctalia-shell.homeModules.default ];

      programs.noctalia-shell = {
        enable = true;
        # noctalia-shell uses Qt/OpenGL (quickshell). On non-NixOS, the Nix-packaged
        # libglvnd cannot find EGL without nixGL injecting LD_LIBRARY_PATH and
        # __EGL_VENDOR_LIBRARY_FILENAMES. Wrap the package so the spawn-at-startup
        # entry in config.kdl resolves to the nixGL wrapper via PATH.
        package = config.lib.nixGL.wrap (
          inputs.noctalia-shell.packages.${pkgs.stdenv.hostPlatform.system}.default
        );
        settings = ./noctalia/settings.json;
        colors = noctaliaColors;
        plugins = ./noctalia/plugins.json;
      };

      xdg.configFile."niri/config.kdl".source = ./config.kdl;
      xdg.configFile."niri/theme.kdl".source = themeFile;

      home.packages = with pkgs; [
        brightnessctl
        cliphist
        imagemagick
        playerctl
        python3
        wireplumber
        wlsunset
      ];
    };
}
