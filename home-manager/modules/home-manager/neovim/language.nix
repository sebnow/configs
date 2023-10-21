{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.neovim = {
      plugins = with pkgs.vimPlugins;
        [
          cmp-nvim-lsp
          go-nvim
          lsp-inlayhints-nvim # TODO :Review - may not be required
          null-ls-nvim
          nvim-lspconfig
          nvim-treesitter
          rust-tools-nvim # TODO: Review - may not be required
          vim-markdown
          vim-terraform
          which-key-nvim
        ]
        ++ (with pkgs.vimPlugins.nvim-treesitter-parsers; [
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
      extraLuaConfig = "require('sebnow.language')";
      extraPackages = with pkgs; [
        golangci-lint-langserver
        gopls
        jsonnet-language-server
        lua-language-server
        nixd
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

    xdg.configFile."nvim/lua/sebnow/language.lua" = {
      source = ./lua/sebnow/language.lua;
    };
  };
}
