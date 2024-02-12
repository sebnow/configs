{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        indent-blankline-nvim
        lualine-nvim
        noice-nvim
        nvim-web-devicons
        oil-nvim
        trouble-nvim
        which-key-nvim
      ];
      extraLuaConfig = "require('sebnow.ui')";
    };

    xdg.configFile."nvim/lua/sebnow/ui.lua" = {
      source = ./lua/sebnow/ui.lua;
    };
  };
}
