local ayu = require("ayu")
local colors = require("ayu.colors")
local mirage = true
colors.generate(mirage)

ayu.setup({
  mirage = mirage,
  overrides = {
    -- Increase comment contrast
    Comment = { fg = "#626A73" },
    FloatBorder = { fg = colors.selection_inactive },
  },
})

ayu.colorscheme()
