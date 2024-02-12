require("dressing").setup()

require("ibl").setup()

require("lualine").setup({
  options = {
    component_separators = "|",
    section_separators = "",
  },
  sections = {
    lualine_a = {
      "mode",
      function()
        local reg = vim.fn.reg_recording()
        if reg == "" then
          return ""
        else
          return "REC @" .. reg
        end
      end,
    },
  },
  extensions = { "fugitive", "quickfix" },
})

require("oil").setup({
  default_file_explorer = true,
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

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

-- Ripped off from https://www.reddit.com/r/neovim/comments/xy0tu1/comment/irfegvd/
vim.api.nvim_create_autocmd("RecordingEnter", {
  callback = function()
    require("lualine").refresh({
      place = { "statusline" },
    })
  end,
})

vim.api.nvim_create_autocmd("RecordingLeave", {
  callback = function()
    -- This is going to seem really weird!
    -- Instead of just calling refresh we need to wait a moment because of the nature of
    -- `vim.fn.reg_recording`. If we tell lualine to refresh right now it actually will
    -- still show a recording occuring because `vim.fn.reg_recording` hasn't emptied yet.
    -- So what we need to do is wait a tiny amount of time (in this instance 50 ms) to
    -- ensure `vim.fn.reg_recording` is purged before asking lualine to refresh.
    local timer = vim.loop.new_timer()
    timer:start(
      50,
      0,
      vim.schedule_wrap(function()
        require("lualine").refresh({
          place = { "statusline" },
        })
      end)
    )
  end,
})
