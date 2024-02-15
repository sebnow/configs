{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        telescope-nvim
        telescope-ui-select-nvim
        which-key-nvim
      ];
      extraLuaConfig = "require('sebnow.telescope')";
      extraPackages = with pkgs; [
        fd
      ];
    };

    xdg.configFile."nvim/lua/sebnow/telescope.lua" = {
      source = ./lua/sebnow/telescope.lua;
    };
  };
}
