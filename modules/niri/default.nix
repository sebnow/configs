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
      pkgs,
      lib,
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
    in
    {
      programs.noctalia = {
        enable = true;
        # noctalia-shell uses Qt/OpenGL (quickshell). On non-NixOS, the Nix-packaged
        # libglvnd cannot find EGL without nixGL injecting LD_LIBRARY_PATH and
        # __EGL_VENDOR_LIBRARY_FILENAMES. Wrap the package so the spawn-at-startup
        # entry in config.kdl resolves to the nixGL wrapper via PATH.
        package =
          if config.targets.genericLinux.enable then config.lib.nixGL.wrap pkgs.noctalia else pkgs.noctalia;
        settings = {
          bar.default = {
            auto_hide = true;
            background_opacity = 0.75;
            reserve_space = false;
            margin_edge = 0;
            margin_ends = 0;
            radius = 0;
            start = [
              "launcher"
              "workspaces"
              "media"
            ];
            center = [ "active_window" ];
            end = [
              "tray"
              "clock"
              "notifications"
              "battery"
              "volume"
              "brightness"
              "control-center"
            ];
          };
          dock.enabled = false;
          notification.background_opacity = 0.75;
          osd.background_opacity = 0.75;
          shell = {
            avatar_path = "${config.home.homeDirectory}/.face";
            polkit_agent = true;
          };
          theme = {
            mode = "dark";
            source = "builtin";
            builtin = "Catppuccin";
          };
          wallpaper = {
            enabled = true;
            directory = "${config.home.homeDirectory}/Pictures/Wallpapers";
            automation = {
              enabled = true;
            };
          };
        };
      };

      xdg.configFile."niri/config.kdl".source = ./config.kdl;
      xdg.configFile."niri/theme.kdl".source = themeFile;

      home.packages = with pkgs; [
        brightnessctl
        cliphist
        playerctl
      ];

      programs.fzf.colors.bg = lib.mkForce "-1";
    };
}
