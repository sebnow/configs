{ ... }:
{
  flake.modules.homeManager.neovim =
    { pkgs, lib, ... }:
    let
      # Override fff-nvim 0.5.1 from nixpkgs to 0.5.2.
      # 0.5.1 crashes on the first keystroke when filtering files in the picker.
      # Remove once nixpkgs updates past v0.5.2.
      fff-nvim-src = pkgs.fetchFromGitHub {
        owner = "dmtrKovalenko";
        repo = "fff.nvim";
        tag = "v0.5.2";
        hash = "sha256-rv33dRf53m9iJwRl56z9oU0EuY1wUChsZyHOi/3gv4A=";
      };

      fff-nvim-lib = pkgs.vimPlugins.fff-nvim.passthru.fff-nvim-lib.overrideAttrs (old: {
        version = "0.5.2";
        src = fff-nvim-src;
        cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
          src = fff-nvim-src;
          name = "fff-nvim-0.5.2-vendor";
          hash = "sha256-ylQtZa3ZRs38mhge5tLLCRpnUdHYSjuZOwU+/6TO8Cw=";
        };
      });

      fff-nvim = pkgs.vimPlugins.fff-nvim.overrideAttrs (old: {
        version = "0.5.2";
        src = fff-nvim-src;
        postPatch = builtins.replaceStrings
          [ "${pkgs.vimPlugins.fff-nvim.passthru.fff-nvim-lib}" ]
          [ "${fff-nvim-lib}" ]
          old.postPatch;
      });

      fff-snacks-nvim = pkgs.vimUtils.buildVimPlugin {
        pname = "fff-snacks.nvim";
        version = "2025-04-04";
        src = pkgs.fetchFromGitHub {
          owner = "madmaxieee";
          repo = "fff-snacks.nvim";
          rev = "05e2db43f054468c0f7d7e43994c03ee560d27b9";
          hash = "sha256-ow2jHY+hOyVoOzmyS5v3l18YHHWb5iUArc4fezgtUpY=";
        };
        dependencies = [
          fff-nvim
          pkgs.vimPlugins.snacks-nvim
        ];
      };
    in
    {
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
          comment-nvim
          conform-nvim
          diffview-nvim
          fff-nvim
          fff-snacks-nvim
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
          (nvim-treesitter.withPlugins (p: with p; [
            bash
            c
            comment
            go
            gomod
            gosum
            gowork
            javascript
            jsdoc
            lua
            luadoc
            luap
            markdown
            markdown_inline
            nix
            proto
            query
            regex
            tsx
            typescript
            vim
            vimdoc
            yaml
            zig
          ]))
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

      catppuccin.nvim.settings = {
        integrations = {
          diffview = true;
          markview = true;
          noice = true;
          snacks = {
            enabled = true;
            indent_scope_color = "surface0";
          };
        };
      };
    };
}
