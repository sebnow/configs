return {
  config = function()
    require("lualine").setup({
      options = {
        component_separators = "|",
        section_separators = "",
      },
      extensions = { "fugitive", "quickfix" },
    })
  end,
}
