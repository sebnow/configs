local config = require("nvim-treesitter.configs")

config.setup({
  ensure_installed = "maintained",
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
    disable = { "yaml" },
  },
})
