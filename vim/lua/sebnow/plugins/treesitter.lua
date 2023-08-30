return {
  "nvim-treesitter/nvim-treesitter",
  commit = "920b37260ebc720b0399bd12954fd2bf8bd18242",
  build = ":TSUpdate",
  config = function()
    local config = require("nvim-treesitter.configs")

    config.setup({
      auto_install = true,
      ensure_installed = {
        "bash",
        "dockerfile",
        "go",
        "gomod",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "nix",
        "python",
        "rust",
        "svelte",
        "toml",
        "typescript",
        "vim",
        "yaml",
      },
      highlight = {
        enable = true,
      },
      indent = {
        enable = true,
        disable = { "yaml" },
      },
    })
  end,
}
