return {
  "jose-elias-alvarez/null-ls.nvim",
  dependencies = {
    "jayp0521/mason-null-ls.nvim",
    "neovim/nvim-lspconfig",
    "nvim-lua/plenary.nvim",
  },
  config = function()
    local nullls = require("null-ls")

    nullls.setup({
      sources = {
        nullls.builtins.formatting.prettier,
        nullls.builtins.formatting.stylua,
      },
    })

    require("mason-null-ls").setup({
      automatic_installation = true,
    })
  end,
}
