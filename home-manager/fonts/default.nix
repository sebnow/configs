{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.fonts.fontconfig;
in {
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      xdg.configFile = {
        "fontconfig/conf.d/50-sad-state-of-font-rendering.conf".source = ./fonts.conf;
      };

      home.packages = with pkgs; [
        iosevka
        nerd-fonts.iosevka-term
      ];
    })
  ];
}
