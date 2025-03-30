{ pkgs, ... }:
{
  imports = [
    ./lang/golang.nix
    ./lang/nix.nix
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
        indent-blankline-nvim
        lualine-nvim
        markview-nvim
        neoscroll-nvim
        noice-nvim
        nvim-lspconfig
        nvim-treesitter-context
        nvim-treesitter.withAllGrammars
        nvim-web-devicons
        oil-nvim
        telescope-nvim
        telescope-ui-select-nvim
        trouble-nvim
        vim-fugitive
        vim-matchup
        vim-terraform
        which-key-nvim
      ];
      extraPackages = with pkgs; [
        fd
        jsonnet-language-server
        lua-language-server
        marksman
        nodePackages.bash-language-server
        nodePackages.prettier
        nodePackages.typescript-language-server
        nodePackages.vscode-langservers-extracted
        nodePackages.yaml-language-server
        python311Packages.python-lsp-server
        shellcheck
        stylua
        terraform-ls
      ];
    };
  };
}
