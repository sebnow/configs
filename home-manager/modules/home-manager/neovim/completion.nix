{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        cmp_luasnip
        cmp-buffer
        cmp-nvim-lsp
        cmp-path
        friendly-snippets
        luasnip
        nvim-cmp
      ];
      extraLuaConfig = "require('sebnow.completion')";
    };

    xdg.configFile."nvim/lua/sebnow/completion.lua" = {
      source = ./lua/sebnow/completion.lua;
    };
  };
}
