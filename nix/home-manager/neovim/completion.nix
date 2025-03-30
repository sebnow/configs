{
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      extraLuaConfig = "require('sebnow.completion')";
    };

    xdg.configFile."nvim/lua/sebnow/completion.lua" = {
      source = ./lua/sebnow/completion.lua;
    };
  };
}
