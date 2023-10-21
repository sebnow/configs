{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        dressing-nvim
        indent-blankline-nvim
        lualine-nvim
        nvim-web-devicons
        trouble-nvim
      ];
      extraLuaConfig = "require('sebnow.ui')";
    };

    xdg.configFile."nvim/lua/sebnow/ui.lua" = {
      source = ./lua/sebnow/ui.lua;
    };
  };
}
