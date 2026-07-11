{ ... }:
{
  flake.modules.homeManager.terminals =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      ghosttyCfg = config.programs.ghostty;
    in
    {
      options.programs.ghostty.isDefault = lib.mkEnableOption "as the default terminal";

      config = lib.mkMerge [
        # Ghostty
        (lib.mkIf ghosttyCfg.enable {
          # Catppuccin nix only supports one theme
          catppuccin.ghostty.enable = false;

          programs.ghostty = {
            package =
              if pkgs.stdenv.isDarwin then
                null
              else if config.targets.genericLinux.enable then
                config.lib.nixGL.wrap pkgs.ghostty
              else
                pkgs.ghostty;
            settings = {
              font-size = if pkgs.stdenv.isDarwin then 14 else 12;
              font-family = "IosevkaTerm NF";
              theme = "light:Catppuccin Latte,dark:Catppuccin Mocha";
              gtk-titlebar-style = "tabs";
              keybind = [
                "shift+enter=text:\\n"
              ]
              ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
                "ctrl+shift+enter=new_split:right"
                "ctrl+shift+d=new_split:down"
                "ctrl+shift+z=toggle_split_zoom"
                "ctrl+shift+[=goto_split:previous"
                "ctrl+shift+]=goto_split:next"
                "ctrl+shift+h=goto_split:left"
                "ctrl+shift+j=goto_split:bottom"
                "ctrl+shift+k=goto_split:top"
                "ctrl+shift+l=goto_split:right"
              ];
            };
          };

          home.sessionVariables.TERMINAL = lib.mkIf ghosttyCfg.isDefault "ghostty";
        })
        (lib.mkIf (ghosttyCfg.enable && pkgs.stdenv.isLinux && config.targets.genericLinux.enable) {
          xdg.configFile."systemd/user/app-com.mitchellh.ghostty.service" = lib.mkForce {
            text =
              let
                unwrappedGhostty = pkgs.ghostty;
                wrappedGhostty = config.lib.nixGL.wrap pkgs.ghostty;
              in
              builtins.replaceStrings [ "${unwrappedGhostty}/bin/ghostty" ] [ "${wrappedGhostty}/bin/ghostty" ] (
                builtins.readFile "${unwrappedGhostty}/share/systemd/user/app-com.mitchellh.ghostty.service"
              );
          };
        })
      ];
    };
}
