{ inputs, ... }:
{
  flake.overlays.neovim = inputs.neovim.overlays.default;

  flake.modules.homeManager.neovim = { pkgs, lib, ... }: {
    imports = [
      ./_lang/bash.nix
      ./_lang/c.nix
      ./_lang/golang.nix
      ./_lang/javascript.nix
      ./_lang/lua.nix
      ./_lang/markdown.nix
      ./_lang/nix.nix
      ./_lang/protobuf.nix
      ./_lang/yaml.nix
      ./_lang/zig.nix
    ];

    programs.neovim = {
      enable = true;
      defaultEditor = true;
      initLua = lib.strings.concatStrings [
        ''
          vim.g.codelldb_path = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb";
        ''
        (builtins.readFile ./config.lua)
      ];
      withPython3 = false;
      withRuby = false;
      withNodeJs = false;
      plugins = with pkgs.vimPlugins; [
        catppuccin-nvim
        comment-nvim
        conform-nvim
        diffview-nvim
        inc-rename-nvim
        lualine-nvim
        markview-nvim
        neogit
        neotest
        neotest-golang
        noice-nvim
        nvim-dap
        nvim-dap-lldb
        nvim-dap-ui
        nvim-lspconfig
        nvim-nio # Required for neotest, nvim-dap-ui
        nvim-treesitter-context
        nvim-treesitter.withAllGrammars
        nvim-web-devicons
        oil-nvim
        plenary-nvim # Required for neogit, neotest
        snacks-nvim
        vim-matchup
        which-key-nvim
      ];
      extraPackages = [
        pkgs.fd
        pkgs.ripgrep
        pkgs.vscode-extensions.vadimcn.vscode-lldb
      ];
    };
  };
}
