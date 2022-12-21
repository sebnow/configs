return {
  "lukas-reineke/indent-blankline.nvim",
  lazy = false,
  config = function()
    require("indent_blankline").setup({
      show_current_context = true,
    })
  end,
}
