vim.opt.laststatus = 3

vim.api.nvim_set_hl(0, "WinSeparator", { fg = require("catppuccin.palettes").get_palette().surface1 })

require("ibl").setup()

require("nvim-web-devicons").setup()

require("lualine").setup({
  options = {
    component_separators = "|",
    section_separators = "",
  },
  sections = {
    lualine_x = {
      {
        require("noice").api.status.message.get_hl,
        cond = require("noice").api.status.message.has,
      },
      {
        require("noice").api.status.mode.get,
        cond = require("noice").api.status.mode.has,
        color = { fg = require("catppuccin.palettes").get_palette().rosewater },
      },
    },
  },
  extensions = { "fugitive", "quickfix", "oil" },
})

require("oil").setup({
  default_file_explorer = true,
})
vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

require("noice").setup({
  cmdline = { view = "cmdline" },
  lsp = {
    override = {
      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
      ["vim.lsp.util.stylize_markdown"] = true,
    },
  },
  presets = {
    bottom_search = true,
    command_palette = false,
    long_message_to_split = true,
    inc_rename = true,
    lsp_doc_border = true,
  },
})
require("telescope").load_extension("noice")

require('neoscroll').setup({
  easing = "sine"
})

local wk = require("which-key")
wk.setup()

wk.add({
  { "<localleader>S", group = "Source Control" },
  { "<localleader>b", group = "Buffers" },
  { "<localleader>d", group = "Diagnostics" },
  { "<localleader>p", group = "Project" },
  { "<localleader>r", group = "Rename" },
  { "<localleader>s", group = "Symbols" },
}, { mode = { "n" } })

wk.add({
  { "<localleader>c", group = "Code Actions" },
  { "<localleader>f", group = "Format" },
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
