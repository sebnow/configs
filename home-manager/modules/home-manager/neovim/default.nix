{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      enable = true;
      defaultEditor = true;
      plugins = with pkgs.vimPlugins; [
        cmp_luasnip
        cmp-nvim-lsp
        friendly-snippets
        rust-tools-nvim # TODO: Review - may not be required
        lsp-inlayhints-nvim # TODO :Review - may not be required
        nvim-web-devicons
        trouble-nvim
        vim-fugitive
        vim-markdown
        vim-terraform
        which-key-nvim
        {
          plugin = dressing-nvim;
          type = "lua";
          config = "require('dressing').setup()";
        }
        {
          plugin = nvim-cmp;
          type = "lua";
          config = "require('sebnow.plugins.cmp').config()";
        }
        {
          plugin = catppuccin-nvim;
          type = "lua";
          config = "require('sebnow.plugins.catppuccin').config()";
        }
        {
          plugin = lualine-nvim;
          type = "lua";
          config = "require('sebnow.plugins.lualine').config()";
        }
        {
          plugin = telescope-nvim;
          type = "lua";
          config = "require('sebnow.plugins.telescope').config()";
        }
        {
          plugin = luasnip;
          type = "lua";
          config = "require('sebnow.plugins.luasnip').config()";
        }
        {
          plugin = go-nvim;
          type = "lua";
          config = "require('sebnow.plugins.go').config()";
        }
        {
          plugin = indent-blankline-nvim;
          type = "lua";
          config = "require('sebnow.plugins.indent-blankline').config()";
        }
        {
          plugin = nvim-lspconfig;
          type = "lua";
          config = "require('sebnow.plugins.lsp').config()";
        }
        # FIXME: Not required since Nix handles dependencies. The lua config is
        # shared between Nix-managed and non-managed, and mason is useful for
        # the latter. Once this is completly migrated to Nix, mason can be
        # removed.
        mason-lspconfig-nvim
        mason-nvim # Don't load the config since mason is not actually used
      ];
      extraLuaConfig = ''
        require('sebnow.settings')
      '';
      extraPackages = with pkgs; [
        golangci-lint-langserver
        gopls
        jsonnet-language-server
        lua-language-server
        nixd
        nodePackages.bash-language-server
        nodePackages.bash-language-server
        nodePackages.prettier
        nodePackages.typescript-language-server
        nodePackages.vscode-langservers-extracted
        nodePackages.yaml-language-server
        python311Packages.python-lsp-server
        stylua
        terraform-ls
        shellcheck
        rust-analyzer
      ];
    };

    #xdg.configFile."nvim/lua" = {
    #  source = ./lua;
    #  recursive = true;
    #};
  };
}
