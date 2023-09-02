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
          plugin = null-ls-nvim;
          type = "lua";
          config = "require('sebnow.plugins.null-ls').config()";
        }
        {
          plugin = nvim-lspconfig;
          type = "lua";
          config = "require('sebnow.plugins.lsp').config()";
        }
        {
            plugin = nvim-treesitter;
            type = "lua";
            config = "require('sebnow.plugins.treesitter').config()";
        }
      ] ++ (with pkgs.vimPlugins.nvim-treesitter-parsers; [
        bash
        dockerfile
        go
        gomod
        html
        javascript
        json
        lua
        markdown
        nix
        python
        rust
        toml
        typescript
        vim
        yaml
      ]);
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
        rust-analyzer
        shellcheck
        stylua
        terraform-ls
      ];
    };

    xdg.configFile."nvim/lua" = {
      source = ./lua;
      recursive = true;
    };
  };
}
