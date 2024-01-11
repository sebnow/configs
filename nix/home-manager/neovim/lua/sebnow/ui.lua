require("dressing").setup()

require("ibl").setup()

require("lualine").setup({
  options = {
    component_separators = "|",
    section_separators = "",
  },
  extensions = { "fugitive", "quickfix" },
})

local wk = require("which-key")
wk.setup()

wk.register({
  S = { name = "Source Control" },
  b = { name = "Buffers" },
  d = { name = "Diagnostics" },
  p = { name = "Project" },
  r = { name = "Rename" },
  s = { name = "Symbols" },
}, { prefix = "<localleader>", mode = { "n" } })

wk.register({
  c = { name = "Code Actions" },
  f = { name = "Format" },
}, { prefix = "<localleader>", mode = { "n", "v" } })
