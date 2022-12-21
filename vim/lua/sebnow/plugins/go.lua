return {
  "ray-x/go.nvim",
  ft = { "go" },
  config = function()
    require("go").setup({
      lsp_inlay_hints = { enable = false },
    })
  end,
}
