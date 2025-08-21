{ pkgs, ... }:
{
  imports = [
    ./lang/bash.nix
    ./lang/golang.nix
    ./lang/javascript.nix
    ./lang/lua.nix
    ./lang/markdown.nix
    ./lang/nix.nix
    ./lang/protobuf.nix
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
        neotest
        neotest-golang
        noice-nvim
        nvim-nio # Required for neotest
        nvim-treesitter-context
        nvim-treesitter.withAllGrammars
        nvim-web-devicons
        oil-nvim
        plenary-nvim # Required for neogit, neotest
        snacks-nvim
        vim-matchup
        which-key-nvim
      ];
      extraPackages = with pkgs; [
        fd
        gofumpt
        golines
        nixfmt
        prettierd
        ripgrep
      ];
    };
  };
}
