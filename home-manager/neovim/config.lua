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
  extensions = {
    "quickfix",
    "oil",
    {
      sections = { lualine_a = { "branch" } },
      filetypes = { "NeogitStatus" },
    },
  },
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
    lsp_doc_border = true,
  },
})

local wk = require("which-key")
wk.setup()

wk.add({
  { "<localleader>D", group = "Debugging" },
  { "<localleader>S", group = "Source Control" },
  { "<localleader>b", group = "Buffers" },
  { "<localleader>d", group = "Diagnostics" },
  { "<localleader>l", group = "LSP" },
  { "<localleader>li", group = "Inlay Hint" },
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
  toggle = {
    enabled = true,
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
  "buf_ls",
  "clangd",
  "golangci_lint_ls",
  "gopls",
  "lua_ls",
  "marksman",
  "nixd",
  "tsserver",
  "yamlls",
  "zls",
})

require("snacks").toggle
  .new({
    id = "Inlay Hints",
    name = "Inlay Hints",
    get = function()
      return vim.lsp.inlay_hint.is_enabled({ bufnr = 0 })
    end,
    set = function(state)
      vim.lsp.inlay_hint.enable(state)
    end,
  })
  :map("<localleader>lit")

vim.lsp.config("lua_ls", {
  settings = {
    -- This is very neovim specific but I don't currently use Lua for
    -- anything else.
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      hint = {
        enable = true,
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
})

vim.lsp.config("gopls", {
  settings = {
    gopls = {
      hints = {
        compositeLiteralFields = true,
        constantValues = true,
        ignoredError = true,
        parameterNames = true,
      },
    },
  },
})

vim.lsp.config("nixd", {
  settings = {
    nixd = {
      formatting = {
        command = { "nixfmt" },
      },
    },
  },
})

vim.lsp.config("yamlls", {
  settings = {
    -- https://github.com/redhat-developer/vscode-redhat-telemetry#how-to-disable-telemetry-reporting
    redhat = { telemetry = { enabled = false } },
  },
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

require("treesitter-context").setup({
  enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
  max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
  line_numbers = true,
  multiline_threshold = 20, -- Maximum number of lines to show for a single context
  mode = "cursor",
})

require("Comment").setup()

require("conform").setup({
  lsp_format = "fallback",
  formatters_by_ft = {
    lua = { "stylua" },
    nix = { "nixfmt" },
    json = { "prettierd", "prettier", stop_after_first = true },
    javascript = { "prettierd", "prettier", stop_after_first = true },
    typescript = { "prettierd", "prettier", stop_after_first = true },
    proto = { "buf" },
    go = { "gofumpt" },
    markdown = { "prettier", "injected" },
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
  format_on_save = function()
    if vim.g.disable_autoformat then
      return
    end

    return {
      timeout_ms = 500,
      lsp_fallback = true,
    }
  end,
})

require("snacks").toggle
  .new({
    id = "Format on Save",
    name = "Format on Save",
    get = function()
      return not vim.g.disable_autoformat
    end,
    set = function(_)
      vim.g.disable_autoformat = not vim.g.disable_autoformat
    end,
  })
  :map("<localleader>ft")

vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"

local floating_preview_opts = {
  focusable = false,
  border = "rounded",
}

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
vim.keymap.set("n", "grc", vim.lsp.codelens.run, { desc = "Run Code Lens" })

vim.keymap.set("n", "<localleader>do", function()
  vim.diagnostic.open_float(floating_preview_opts)
end, { desc = "Show line diagnostics" })

vim.keymap.set("n", "<localleader>fb", function()
  require("conform").format()
end, { desc = "Format buffer" })

vim.keymap.set("v", "<localleader>fb", function()
  require("conform").format()
end, { desc = "Format range" })

require("neogit").setup({
  integrations = {
    snacks = true,
    diffview = true,
  },
})

require("neotest").setup({
  adapters = {
    require("neotest-golang")({
      go_test_args = { "-race" },
    }),
  },
})

wk.add({
  { "<localleader>t", group = "Testing" },
}, { mode = { "n" } })

vim.keymap.set("n", "<localleader>tr", function()
  require("neotest").run.run()
end, { desc = "Run nearest test" })

vim.keymap.set("n", "<localleader>tl", function()
  require("neotest").run.run_last()
end, { desc = "Run last test" })

vim.keymap.set("n", "<localleader>tt", function()
  require("neotest").run.run(vim.fn.expand("%"))
end, { desc = "Run test file" })

vim.keymap.set("n", "<localleader>tT", function()
  require("neotest").run.run(vim.uv.cwd())
end, { desc = "Run all test files" })

require("dap-lldb").setup({
  codelldb_path = vim.g.codelldb_path,
})

local dap = require("dap")
dap.configurations.zig = {
  {
    name = "Launch Debugger",
    type = "lldb",
    request = "launch",
    cwd = "${workspaceFolder}",
    program = function()
      return require("dap.utils").pick_file({ path = "zig-out/" })
    end,
    stopOnEntry = false,
  },
}

local dapui = require("dapui")
dapui.setup()

dap.listeners.before.attach.dapui_config = function()
  dapui.open()
end
dap.listeners.before.launch.dapui_config = function()
  dapui.open()
end
dap.listeners.before.event_terminated.dapui_config = function()
  dapui.close()
end
dap.listeners.before.event_exited.dapui_config = function()
  dapui.close()
end

vim.keymap.set("n", "<F5>", function()
  require("dap").continue()
end, { desc = "Continue" })
vim.keymap.set("n", "<S-F5>", function()
  require("dap").terminate()
end, { desc = "Terminate" })
vim.keymap.set("n", "<F10>", function()
  require("dap").step_over()
end, { desc = "Step over" })
vim.keymap.set("n", "<F11>", function()
  require("dap").step_into()
end, { desc = "Step into" })
vim.keymap.set("n", "<F12>", function()
  require("dap").step_out()
end, { desc = "Step out" })
vim.keymap.set("n", "<Leader>Db", function()
  require("dap").toggle_breakpoint()
end, { desc = "Toggle breakpoint" })
vim.keymap.set("n", "<Leader>DB", function()
  require("dap").set_breakpoint()
end, { desc = "Set breakpoint" })
vim.keymap.set("n", "<Leader>Dlp", function()
  require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end, { desc = "Set log point" })
vim.keymap.set("n", "<Leader>Dr", function()
  require("dap").repl.open()
end, { desc = "Open REPL" })
vim.keymap.set("n", "<Leader>Dl", function()
  require("dap").run_last()
end, { desc = "Run last" })
vim.keymap.set({ "n", "v" }, "<Leader>Dh", function()
  require("dap.ui.widgets").hover()
end, { desc = "Hover" })
vim.keymap.set({ "n", "v" }, "<Leader>Dp", function()
  require("dap.ui.widgets").preview()
end, { desc = "Preview" })
vim.keymap.set("n", "<Leader>Df", function()
  local widgets = require("dap.ui.widgets")
  widgets.centered_float(widgets.frames)
end, { desc = "Show frames" })
vim.keymap.set("n", "<Leader>Ds", function()
  local widgets = require("dap.ui.widgets")
  widgets.centered_float(widgets.scopes)
end, { desc = "Show scopes" })
vim.keymap.set("n", "<Leader>DT", function()
  require("dapui").toggle()
end, { desc = "Toggle UI" })
