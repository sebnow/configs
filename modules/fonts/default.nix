{ ... }:
{
  flake.modules.homeManager.fonts =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      config = lib.mkMerge [
        (lib.mkIf config.fonts.fontconfig.enable {
          xdg.configFile = {
            "fontconfig/conf.d/50-sad-state-of-font-rendering.conf".source = ./fonts.conf;
          };

          home.packages = with pkgs; [
            iosevka
            nerd-fonts.iosevka-term
          ];
        })
      ];
    };
}
