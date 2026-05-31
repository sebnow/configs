{ inputs, ... }:
{
  # niri binary is installed via pacman; this module manages only user-scoped
  # configuration and tooling. niri-flake is intentionally excluded to avoid
  # its gnome-keyring forced-enable, which conflicts with KeePassXC.
  flake.modules.homeManager.niri =
    { pkgs, ... }:
    {
      imports = [ inputs.noctalia-shell.homeModules.default ];

      programs.noctalia-shell.enable = true;

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
