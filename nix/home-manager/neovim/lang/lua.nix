{pkgs, ...}: {
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        lua-language-server
        stylua
      ];
    };
    xdg.configFile."nvim/lsp/lua_ls.lua".source = ./lua_ls.lua;
  };
}
