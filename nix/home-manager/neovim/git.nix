{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        vim-fugitive
      ];
    };
  };
}
