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
  { "<localleader>D", group = "Diagnostics" },
  { "<localleader>S", group = "Source Control" },
  { "<localleader>b", group = "Buffers" },
  { "<localleader>p", group = "Project" },
  { "<localleader>s", group = "Symbols" },
  { "<localleader>u", group = "UI Toggles" },
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

vim.keymap.set("n", "<C-p>", makePicker("files"), { desc = "Find files" })
vim.keymap.set("n", "<localleader>;", makePicker("command_history"), { desc = "Explore command history" })
vim.keymap.set("n", "<localleader>be", makePicker("buffers"), { desc = "Explore buffers" })
vim.keymap.set("n", "<localleader>l", makePicker("resume"), { desc = "Resume previous list" })
vim.keymap.set("n", "<localleader>sw", makePicker("lsp_workspace_symbols"), { desc = "Explore workspace symbols" })
vim.keymap.set("n", "<localleader>p/", makePicker("grep"), { desc = "Search in project" })
vim.keymap.set("v", "<localleader>p/", makePicker("grep_word"), { desc = "Search word in project" })
vim.keymap.set("n", "<localleader>Sb", makePicker("git_branches"), { desc = "Branches" })
vim.keymap.set("n", "<localleader>Dl", makePicker("diagnostics"), { desc = "Explore diagnostics" })
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
  "ts_ls",
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
  :map("<localleader>uh")

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

vim.api.nvim_create_autocmd("FileType", {
  callback = function(ev)
    if vim.treesitter.get_parser(0, nil, { error = false }) then
      vim.treesitter.start(ev.buf)
      if vim.bo.filetype ~= "yaml" then
        vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
      end
    end
  end,
})

vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function()
    if vim.bo.filetype == "c" and vim.treesitter.get_parser(0, nil, { error = false }) then
      vim.wo.foldmethod = "expr"
      vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
    else
      vim.wo.foldmethod = vim.go.foldmethod
      vim.wo.foldexpr = vim.go.foldexpr
    end
  end,
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

vim.keymap.set("n", "grc", vim.lsp.codelens.run, { desc = "Run Code Lens" })

vim.keymap.set("n", "<localleader>Do", function()
  vim.diagnostic.open_float(floating_preview_opts)
end, { desc = "Show line diagnostics" })

vim.keymap.set("n", "<localleader>fb", function()
  require("conform").format()
end, { desc = "Format buffer" })

vim.keymap.set("v", "<localleader>fb", function()
  require("conform").format()
end, { desc = "Format range" })

require("jj").setup({
  picker = { snacks = {} },
  editor = { auto_insert = true },
  diff = { backend = "diffview" },
})

local jj_cmd = require("jj.cmd")
local jj_picker = require("jj.picker")
vim.keymap.set("n", "<localleader>Sc", jj_cmd.commit, { desc = "Commit" })
vim.keymap.set("n", "<localleader>Sd", jj_cmd.describe, { desc = "Describe" })
vim.keymap.set("n", "<localleader>Se", jj_cmd.edit, { desc = "Edit change" })
vim.keymap.set("n", "<localleader>Sf", jj_cmd.fetch, { desc = "Fetch" })
vim.keymap.set("n", "<localleader>Sl", jj_cmd.log, { desc = "Log" })
vim.keymap.set("n", "<localleader>SL", function()
  jj_cmd.log({ revisions = "'all()'" })
end, { desc = "Log all" })
vim.keymap.set("n", "<localleader>Sn", jj_cmd.new, { desc = "New change" })
vim.keymap.set("n", "<localleader>Sp", jj_cmd.push, { desc = "Push" })
vim.keymap.set("n", "<localleader>Sq", jj_cmd.squash, { desc = "Squash" })
vim.keymap.set("n", "<localleader>Ss", jj_cmd.status, { desc = "Status" })
vim.keymap.set("n", "<localleader>Su", jj_cmd.undo, { desc = "Undo" })
vim.keymap.set("n", "<localleader>Sv", jj_picker.status, { desc = "View status" })

require("neogit").setup({
  integrations = {
    snacks = true,
    diffview = true,
  },
})

local diff_base = nil -- nil uses the per-backend default (jj: @-, git: HEAD)
local minidiff_attached_buffers = {} -- buf_id -> { is_jj }

local function set_diff_ref(buf_id)
  local diff = require("mini.diff")
  local info = minidiff_attached_buffers[buf_id]
  if not info then
    return
  end
  local path = vim.api.nvim_buf_get_name(buf_id)
  local dir = vim.fs.dirname(path)
  local name = vim.fs.basename(path)
  local cmd
  if info.is_jj then
    cmd = { "jj", "file", "show", "-r", diff_base or "@-", name }
  else
    cmd = { "git", "show", (diff_base or "HEAD") .. ":./" .. name }
  end

  vim.system(cmd, { cwd = dir, text = true }, function(out)
    if out.code ~= 0 then
      return
    end
    vim.schedule(function()
      if vim.api.nvim_buf_is_valid(buf_id) then
        diff.set_ref_text(buf_id, out.stdout)
      end
    end)
  end)
end

require("mini.diff").setup({
  view = { style = "sign" },
  source = {
    name = "vcs",
    attach = function(buf_id)
      local path = vim.api.nvim_buf_get_name(buf_id)
      if path == "" or vim.bo[buf_id].buftype ~= "" then
        return
      end
      vim.system({ "jj", "root" }, { cwd = vim.fs.dirname(path) }, function(root)
        minidiff_attached_buffers[buf_id] = { is_jj = root.code == 0 }
        vim.schedule(function()
          set_diff_ref(buf_id)
        end)
      end)
      vim.api.nvim_create_autocmd({ "BufWritePost", "FocusGained" }, {
        buffer = buf_id,
        group = vim.api.nvim_create_augroup("MiniDiffVcs_" .. buf_id, { clear = true }),
        callback = function()
          set_diff_ref(buf_id)
        end,
      })
    end,
    detach = function(buf_id)
      minidiff_attached_buffers[buf_id] = nil
      pcall(vim.api.nvim_del_augroup_by_name, "MiniDiffVcs_" .. buf_id)
    end,
  },
})

local function refresh_diff()
  for buf_id in pairs(minidiff_attached_buffers) do
    if vim.api.nvim_buf_is_valid(buf_id) then
      set_diff_ref(buf_id)
    end
  end
end

vim.api.nvim_create_user_command("DiffBase", function(opts)
  diff_base = opts.args ~= "" and opts.args or nil
  refresh_diff()
end, { nargs = "?", desc = "Set VCS diff base revision (empty resets to default)" })

-- Pick the diff base from the VCS log so signs and :DiffFiles retarget to the
-- chosen revision without typing it out.
local function diff_base_picker()
  local dir = vim.uv.cwd()
  local is_jj = vim.system({ "jj", "root" }, { cwd = dir }):wait().code == 0
  local log_cmd = is_jj
      and {
        "jj",
        "log",
        "--no-graph",
        "--limit",
        "50",
        "-T",
        'change_id.shortest() ++ " " ++ if(description, description.first_line(), "(no description)") ++ "\n"',
      }
    or { "git", "log", "--format=%h %s", "-n", "50" }
  local out = vim.system(log_cmd, { cwd = dir, text = true }):wait()
  if out.code ~= 0 then
    vim.notify("DiffBasePick: " .. (out.stderr or "failed to list revisions"), vim.log.levels.ERROR)
    return
  end
  local items = {}
  for line in vim.gsplit(out.stdout or "", "\n", { trimempty = true }) do
    local rev = line:match("^(%S+)")
    if rev then
      items[#items + 1] = { text = line, rev = rev, cwd = dir }
    end
  end
  require("snacks.picker").pick({
    items = items,
    format = "text",
    title = "Set diff base",
    preview = function(ctx)
      local cmd = is_jj and { "jj", "show", "--git", ctx.item.rev } or { "git", "show", ctx.item.rev }
      return Snacks.picker.preview.cmd(cmd, ctx, { ft = "git" })
    end,
    confirm = function(picker, item)
      picker:close()
      if item then
        diff_base = item.rev
        refresh_diff()
        vim.notify("Diff base set to " .. item.rev)
      end
    end,
  })
end

vim.api.nvim_create_user_command("DiffBasePick", diff_base_picker, {
  desc = "Pick the diff base revision from the VCS log",
})
vim.keymap.set("n", "<localleader>Sr", diff_base_picker, { desc = "Set diff base (pick revision)" })

-- Pick files changed in the current diff base range. Each entry previews the
-- per-file diff (so added/deleted files render too) and opens the real file on
-- confirm when it still exists. Uses the same base as the signs, so it stays in
-- sync with :DiffBase.
local function diff_files_picker()
  local dir = vim.uv.cwd()
  local is_jj = vim.system({ "jj", "root" }, { cwd = dir }):wait().code == 0
  local root_cmd = is_jj and { "jj", "root" } or { "git", "rev-parse", "--show-toplevel" }
  local root = vim.trim(vim.system(root_cmd, { cwd = dir, text = true }):wait().stdout or "")
  if root == "" then
    return
  end
  local base = diff_base or (is_jj and "@-" or "HEAD")
  local list_cmd = is_jj and { "jj", "diff", "--from", base, "--to", "@", "--summary" }
    or { "git", "diff", "--name-status", base }
  local out = vim.system(list_cmd, { cwd = root, text = true }):wait()
  if out.code ~= 0 then
    vim.notify("DiffFiles: " .. (out.stderr or "failed to list changes"), vim.log.levels.ERROR)
    return
  end
  local items = {}
  for line in vim.gsplit(out.stdout or "", "\n", { trimempty = true }) do
    -- jj: "M path"   git: "M<TAB>path" (renames: "R100<TAB>old<TAB>new")
    local status, rest = line:match("^(%S+)%s+(.*)$")
    if status then
      local path = rest:match("([^\t]*)$") -- last field = new path on rename
      items[#items + 1] = {
        text = status .. " " .. path,
        file = root .. "/" .. path,
        cwd = root,
        path = path,
        deleted = status:sub(1, 1) == "D",
      }
    end
  end
  if #items == 0 then
    vim.notify("DiffFiles: no changes in range", vim.log.levels.INFO)
    return
  end
  require("snacks.picker").pick({
    items = items,
    format = "text",
    title = "Changed files (" .. base .. "..)",
    preview = function(ctx)
      local cmd = is_jj and { "jj", "diff", "--git", "--from", base, "--to", "@", "--", ctx.item.path }
        or { "git", "diff", base, "--", ctx.item.path }
      return Snacks.picker.preview.cmd(cmd, ctx, { ft = "diff" })
    end,
    confirm = function(picker, item)
      picker:close()
      if item and not item.deleted then
        vim.cmd.edit(vim.fn.fnameescape(item.file))
      elseif item then
        vim.notify(item.path .. " was deleted in this range", vim.log.levels.INFO)
      end
    end,
  })
end

vim.api.nvim_create_user_command("DiffFiles", diff_files_picker, {
  desc = "Pick files changed in the diff base range",
})
vim.keymap.set("n", "<localleader>SH", diff_files_picker, { desc = "Changed files in range" })

-- Pick individual hunks changed since the diff base, across every changed file.
-- The whole-range diff is parsed into one entry per hunk; the preview shows just
-- that hunk and confirm jumps to it. Uses the same base as the signs, so it
-- stays in sync with :DiffBase (and mini.diff).
local function diff_hunks_picker()
  local dir = vim.uv.cwd()
  local is_jj = vim.system({ "jj", "root" }, { cwd = dir }):wait().code == 0
  local root_cmd = is_jj and { "jj", "root" } or { "git", "rev-parse", "--show-toplevel" }
  local root = vim.trim(vim.system(root_cmd, { cwd = dir, text = true }):wait().stdout or "")
  if root == "" then
    return
  end
  local base = diff_base or (is_jj and "@-" or "HEAD")
  local diff_cmd = is_jj and { "jj", "diff", "--git", "--from", base, "--to", "@" }
    or { "git", "diff", base }
  local out = vim.system(diff_cmd, { cwd = root, text = true }):wait()
  if out.code ~= 0 then
    vim.notify("DiffHunks: " .. (out.stderr or "failed to diff"), vim.log.levels.ERROR)
    return
  end

  -- Walk the git-format patch: track the current file from its header, then emit
  -- an item for each "@@ ... +new_start" hunk. Body lines (context/+/-/no-newline)
  -- are appended to the current hunk's preview text.
  local items = {}
  local path, deleted, old_path, hunk
  for line in vim.gsplit(out.stdout or "", "\n", { plain = true }) do
    if line:sub(1, 11) == "diff --git " then
      path, deleted, old_path, hunk = nil, false, nil, nil
    elseif line:sub(1, 4) == "--- " then
      old_path = line:match("^%-%-%- a/(.*)$")
    elseif line:sub(1, 4) == "+++ " then
      if line == "+++ /dev/null" then
        deleted, path = true, old_path
      else
        path = line:match("^%+%+%+ b/(.*)$")
      end
    elseif line:sub(1, 3) == "@@ " and path then
      local new_start, ctx = line:match("^@@ %-[%d,]+ %+(%d+)[,%d]* @@ ?(.*)$")
      if new_start then
        local n = tonumber(new_start)
        hunk = {
          text = vim.trim(string.format("%s:%d %s", path, n, ctx)),
          file = root .. "/" .. path,
          path = path,
          line = n,
          deleted = deleted,
          preview = { text = line, ft = "diff", loc = false },
        }
        items[#items + 1] = hunk
      end
    elseif hunk and line:match("^[ +\\-]") then
      hunk.preview.text = hunk.preview.text .. "\n" .. line
    end
  end

  if #items == 0 then
    vim.notify("DiffHunks: no changes in range", vim.log.levels.INFO)
    return
  end

  require("snacks.picker").pick({
    items = items,
    format = "text",
    title = "Changed hunks (" .. base .. "..)",
    preview = function(ctx)
      return Snacks.picker.preview.preview(ctx)
    end,
    confirm = function(picker, item)
      picker:close()
      if not item then
        return
      end
      if item.deleted then
        vim.notify(item.path .. " was deleted in this range", vim.log.levels.INFO)
        return
      end
      vim.cmd.edit(vim.fn.fnameescape(item.file))
      pcall(vim.api.nvim_win_set_cursor, 0, { item.line, 0 })
    end,
  })
end

vim.api.nvim_create_user_command("DiffHunks", diff_hunks_picker, {
  desc = "Pick hunks changed in the diff base range",
})
vim.keymap.set("n", "<localleader>Sh", diff_hunks_picker, { desc = "Changed hunks in range" })

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

