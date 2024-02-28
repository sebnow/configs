{
  config,
  pkgs,
  ...
}: {
  config = {
    # Copilot-lua requires node to be available
    home.packages = [pkgs.nodejs];
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        cmp-buffer
        cmp-nvim-lsp
        cmp-path
        copilot-cmp
        copilot-lua
        nvim-cmp
      ];
      extraLuaConfig = "require('sebnow.completion')";
    };

    xdg.configFile."nvim/lua/sebnow/completion.lua" = {
      source = ./lua/sebnow/completion.lua;
    };
  };
}
