local is_fresh_install = false
local install_path = vim.call("stdpath", "data") .. "/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_fresh_install = true
  vim.cmd({ cmd = "!", args = { "git", "clone", "https://github.com/wbthomason/packer.nvim.git", install_path } })
  vim.cmd.packadd("packer.nvim")
end

vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])

local packer = require("packer")

local M = packer.startup(function(use)
  use("aklt/plantuml-syntax")
  use("folke/lsp-colors.nvim")
  use("hashivim/vim-terraform")
  use("LnL7/vim-nix")
  use("tpope/vim-fugitive")
  use("wbthomason/packer.nvim")
  use("evanleck/vim-svelte")
  use("simrat39/rust-tools.nvim")
  use("stevearc/dressing.nvim")

  use({
    "hrsh7th/nvim-cmp",
    requires = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
      "L3MON4D3/LuaSnip",
    },
    config = function()
      require("sebnow.plugins.cmp")
    end,
  })

  use({
    "neovim/nvim-lspconfig",
    commit = "da7461b596d70fa47b50bf3a7acfaef94c47727d",
    requires = {
      "folke/which-key.nvim",
      "hrsh7th/cmp-nvim-lsp",
      "simrat39/rust-tools.nvim",
      "lvimuser/lsp-inlayhints.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    config = function()
      require("sebnow.plugins.lsp")
    end,
  })

  use({
    "williamboman/mason.nvim",
    config = function()
      require("sebnow.plugins.mason")
    end,
  })

  -- This will be obsolete once https://github.com/neovim/neovim/issues/18086 is implemented
  use({
    "lvimuser/lsp-inlayhints.nvim",
    config = function()
      require("lsp-inlayhints").setup()
    end,
  })

  use({
    "L3MON4D3/LuaSnip",
    requires = {
      "rafamadriz/friendly-snippets",
    },
    config = function()
      require("sebnow.plugins.luasnip")
    end,
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    commit = "e2efbb6569dbe50e6604cfc2d5d0819eb07d5623",
    run = ":TSUpdate",
    config = function()
      require("sebnow.plugins.treesitter")
    end,
  })

  use({
    "plasticboy/vim-markdown",
    ft = { "markdown" },
  })

  use({
    "ray-x/go.nvim",
    requires = {
      "folke/which-key.nvim",
    },
    config = function()
      require("sebnow.plugins.go")
    end,
  })

  use({
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    requires = {
      "kyazdani42/nvim-web-devicons",
      "nvim-lua/plenary.nvim",
      "nvim-lua/popup.nvim",
      "folke/which-key.nvim",
    },
    config = function()
      require("sebnow.plugins.telescope")
    end,
  })

  use({
    "Shatur/neovim-ayu",
    config = function()
      require("sebnow.plugins.ayu")
    end,
  })

  use({
    "folke/trouble.nvim",
    requires = {
      "kyazdani42/nvim-web-devicons",
    },
  })

  use({
    "jose-elias-alvarez/null-ls.nvim",
    requires = {
      "jayp0521/mason-null-ls.nvim",
      "neovim/nvim-lspconfig",
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("sebnow.plugins.null-ls")
    end,
  })

  use({
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("sebnow.plugins.indent-blankline")
    end,
  })
end)

if is_fresh_install then
  packer.sync()
end

return M
