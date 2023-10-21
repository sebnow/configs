{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        catppuccin-nvim
      ];
      extraLuaConfig = "require('sebnow.colors')";
    };

    xdg.configFile."nvim/lua/sebnow/colors.lua" = {
      source = ./lua/sebnow/colors.lua;
    };
  };
}
