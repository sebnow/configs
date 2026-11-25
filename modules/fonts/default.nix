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
          fonts.fontconfig = {
            antialiasing = true;
            hinting = "slight";
            subpixelRendering = "rgb";
          };

          home.packages = with pkgs; [
            font-bitstream-type1
            iosevka
            lato
            nerd-fonts.iosevka-term
            raleway
          ];

          fonts.fontconfig.defaultFonts = {
            monospace = [ "Iosevka" ];
            sansSerif = [ "Lato" ];
            serif = [ "Bitstream Charter" ];
          };

          gtk.font = {
            name = "Lato";
            size = 11;
          };

          dconf.settings."org/gnome/desktop/interface".font-name = "Lato 11";
        })
      ];
    };
}
