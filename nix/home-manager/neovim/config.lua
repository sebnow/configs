vim.o.showmode = false
vim.o.title = true
vim.o.ruler = true
vim.opt.shortmess:append("acI")

vim.o.wildmode = "list:longest"
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true -- Modern languages are opinionated and tend to default to spaces
vim.o.textwidth = 72
vim.opt.formatoptions:append("cronql1jp")
vim.opt.completeopt = { "fuzzy", "menuone", "noinsert" }
vim.opt.completefuzzycollect = "keyword,files,whole_line"

vim.o.smartcase = true
vim.o.ignorecase = true
vim.o.incsearch = true

vim.o.scrolloff = 2
vim.o.sidescrolloff = 5

vim.o.statusline = "%-3.3n %f%( %r%)%( %#WarningMsg#%m%0*%)%=(%l, %c) %P [%{&encoding}:%{&fileformat}]%( %w%) %y "

vim.o.foldenable = true
vim.o.foldmethod = "marker"
vim.o.foldlevel = 99

vim.o.guifont = "IosevkaTerm NFM:h12,Iosevka Term:h12,monospace"
vim.o.timeoutlen = 250

vim.g.netrw_banner = 0
vim.o.cmdheight = 0
vim.opt.laststatus = 3
vim.o.winborder = "rounded"

vim.diagnostic.config({
  virtual_text = false,
  virtual_lines = {
    current_line = true,
  },
  signs = true,
  update_in_insert = false,
})

require("catppuccin").setup({
  integrations = {
    markview = true,
    noice = true,
    snacks = {
      enabled = true,
      indent_scope_color = "surface0",
    },
  },
})

vim.api.nvim_set_hl(0, "WinSeparator", { fg = require("catppuccin.palettes").get_palette().surface1 })

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
    lsp_doc_border = false,
  },
})

local wk = require("which-key")
wk.setup()

wk.add({
  { "<localleader>S", group = "Source Control" },
  { "<localleader>b", group = "Buffers" },
  { "<localleader>d", group = "Diagnostics" },
  { "<localleader>p", group = "Project" },
  { "<localleader>s", group = "Symbols" },
}, { mode = { "n" } })

wk.add({
  { "<localleader>f", group = "Format" },
}, { prefix = "<localleader>", mode = { "n", "v" } })

require("snacks").setup({
  indent = { enabled = true },
  scroll = {
    enabled = true,
    animate = {
      easing = "outQuad",
      duration = { step = 50, total = 125 },
    },
  },
  picker = {
    enabled = true,
    ui_select = true,
    sources = {
      git_branches = {
        layout = { preset = "select" },
      },
      grep = {
        layout = { preset = "ivy" },
      },
    },
    layout = { preset = "telescope" },
  },
})

local makePicker = function(source, opts)
  return function()
    require("snacks.picker").pick(source, opts)
  end
end

vim.keymap.set("n", "<C-p>", makePicker("smart"), { desc = "Find files" })
vim.keymap.set("n", "<localleader>;", makePicker("command_history"), { desc = "Explore command history" })
vim.keymap.set("n", "<localleader>be", makePicker("buffers"), { desc = "Explore buffers" })
vim.keymap.set("n", "<localleader>l", makePicker("resume"), { desc = "Resume previous list" })
vim.keymap.set("n", "<localleader>sw", makePicker("lsp_workspace_symbols"), { desc = "Explore workspace symbols" })
vim.keymap.set("n", "<localleader>p/", makePicker("grep"), { desc = "Search in project" })
vim.keymap.set("n", "<localleader>Sb", makePicker("git_branches"), { desc = "Branches" })
vim.keymap.set("n", "<localleader>dl", makePicker("diagnostics"), { desc = "Explore diagnostics" })
-- Override builtin keymaps to have Snacks.picker provide the list. Not sure why
-- `vim.ui.select` doesn't get used by default.
vim.keymap.set("n", "gO", makePicker("lsp_symbols"), { desc = "Explore document symbols" })
vim.keymap.set("n", "grr", makePicker("lsp_references"), { desc = "Explore symbol references" })

-- Enable built-in LSP auto-completion
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client ~= nil and client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    end
  end,
})

vim.lsp.enable({
  "bashls",
  "golangci_lint_ls",
  "gopls",
  "lua_ls",
  "marksman",
  "nixd",
  "yamlls",
  "zls",
})

vim.filetype.add({ extension = { templ = "templ" } })

require("nvim-treesitter.configs").setup({
  ensure_installed = {}, -- Nix manages parsers
  auto_install = false,
  sync_install = false,
  ignore_install = { "all" },
  modules = {},

  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
    disable = { "yaml" },
  },
  matchup = {
    enable = true,
  },
})

require("Comment").setup()

require("conform").setup({
  formatters_by_ft = {
    lua = { "stylua" },
    nix = { "nixfmt" },
  },
  formatters = {
    nixfmt = {
      command = "nixfmt",
      args = { "$FILENAME" },
      stdin = false,
      condition = function(ctx)
        return vim.fs.find({ "flake.nix" }, { upward = true, path = ctx.dirname })[1]
      end,
    },
  },
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
})

vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"

local floating_preview_opts = {
  focusable = false,
  border = "rounded",
}

local function merge(a, b)
  return vim.tbl_extend("force", a, b)
end

local opts = {}

require("markview").setup({
  preview = {
    icon_provider = "devicons",
  },
})

require("go").setup({
  lsp_inlay_hints = { enable = true },
})

vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Go to declaration" })
vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Go to definition" })

vim.keymap.set("n", "<localleader>do", function()
  vim.diagnostic.open_float(floating_preview_opts)
end, { desc = "Show line diagnostics" })

vim.keymap.set("n", "<localleader>fb", function()
  require("conform").format({ lsp_fallback = true })
end, { desc = "Format buffer" })

vim.keymap.set("v", "<localleader>fb", function()
  local start_row, _ = unpack(vim.api.nvim_buf_get_mark(0, "<"))
  local end_row, _ = unpack(vim.api.nvim_buf_get_mark(0, ">"))
  require("conform").format({
    lsp_fallback = true,
    range = {
      ["start"] = { start_row, 0 },
      ["end"] = { end_row, 0 },
    },
  })
end, { desc = "Format range" })
