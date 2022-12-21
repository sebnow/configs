return {
  "nvim-lualine/lualine.nvim",
  lazy = false,
  dependencies = {
    "kyazdani42/nvim-web-devicons",
    "tpope/vim-fugitive",
    "Shatur/neovim-ayu",
  },
  config = function()
    require("lualine").setup({
      options = {
        theme = "ayu_mirage",
        component_separators = "|",
        section_separators = "",
      },
      extensions = { "fugitive", "quickfix" },
    })
  end,
}
