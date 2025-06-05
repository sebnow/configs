{ pkgs, ... }:
{
  imports = [
    ./lang/bash.nix
    ./lang/golang.nix
    ./lang/javascript.nix
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
        diffview-nvim
        go-nvim
        inc-rename-nvim
        lualine-nvim
        markview-nvim
        neogit
        noice-nvim
        nvim-treesitter-context
        nvim-treesitter.withAllGrammars
        nvim-web-devicons
        oil-nvim
        plenary-nvim # Required for neogit
        snacks-nvim
        vim-matchup
        which-key-nvim
      ];
      extraPackages = with pkgs; [
        fd
        prettierd
        ripgrep
      ];
    };
  };
}
