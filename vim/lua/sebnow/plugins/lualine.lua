require("lualine").setup({
  options = {
    theme = "ayu_mirage",
    component_separators = "|",
    section_separators = "",
  },
  extensions = { "fugitive", "quickfix" },
})
