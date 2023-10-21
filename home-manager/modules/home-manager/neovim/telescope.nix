{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        telescope-nvim
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
