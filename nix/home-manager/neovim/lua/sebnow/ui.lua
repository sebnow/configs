require("dressing").setup()

require("ibl").setup()

require("lualine").setup({
  options = {
    component_separators = "|",
    section_separators = "",
  },
  extensions = { "fugitive", "quickfix" },
})
