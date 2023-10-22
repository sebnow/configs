require("dressing").setup()

require("indent_blankline").setup({
  show_current_context = true,
})

require("lualine").setup({
  options = {
    component_separators = "|",
    section_separators = "",
  },
  extensions = { "fugitive", "quickfix" },
})
