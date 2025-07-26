{ pkgs, ... }:
{
  imports = [
    ./lang/bash.nix
    ./lang/golang.nix
    ./lang/lua.nix
    ./lang/markdown.nix
    ./lang/nix.nix
    ./lang/yaml.nix
    ./lang/zig.nix
  ];
  config = {
    programs.neovim = {
      enable = true;
      defaultEditor = true;
      extraLuaConfig = builtins.readFile ./config.lua;
      plugins = with pkgs.vimPlugins; [
        catppuccin-nvim
        comment-nvim
        conform-nvim
        go-nvim
        inc-rename-nvim
        indent-blankline-nvim
        lualine-nvim
        markview-nvim
        neoscroll-nvim
        noice-nvim
        nvim-treesitter-context
        nvim-treesitter.withAllGrammars
        nvim-web-devicons
        oil-nvim
        telescope-nvim
        telescope-ui-select-nvim
        vim-fugitive
        vim-matchup
        which-key-nvim
      ];
      extraPackages = with pkgs; [
        fd
        ripgrep
      ];
    };
  };
}
