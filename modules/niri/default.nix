{ inputs, ... }:
{
  # niri binary is installed via pacman; this module manages only user-scoped
  # configuration and tooling. niri-flake is intentionally excluded to avoid
  # its gnome-keyring forced-enable, which conflicts with KeePassXC.
  flake.modules.homeManager.niri =
    { config, pkgs, ... }:
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
        plugins = ./noctalia/plugins.json;
      };

      xdg.configFile."niri/config.kdl".source = ./config.kdl;

      home.packages = with pkgs; [
        brightnessctl
        imagemagick
        python3
        cliphist
        wlsunset
      ];
    };
}
