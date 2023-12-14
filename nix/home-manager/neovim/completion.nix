{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        cmp-buffer
        cmp-nvim-lsp
        cmp-path
        nvim-cmp
      ];
      extraLuaConfig = "require('sebnow.completion')";
    };

    xdg.configFile."nvim/lua/sebnow/completion.lua" = {
      source = ./lua/sebnow/completion.lua;
    };
  };
}
