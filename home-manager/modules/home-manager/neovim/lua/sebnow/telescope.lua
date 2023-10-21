local wk = require("which-key")
local actions = require("telescope.actions")
local previewers = require("telescope.previewers")
local sorters = require("telescope.sorters")
local builtin = require("telescope.builtin")
local telescope = require("telescope")

telescope.setup({
  defaults = {
    path_display = { "truncate" },
    mappings = {
      i = {
        ["<C-s>"] = actions.select_horizontal,
        ["<C-x>"] = false,
      },
    },
    file_sorter = sorters.get_fzy_sorter,
    file_previewer = previewers.vim_buffer_cat.new,
    grep_previewer = previewers.vim_buffer_vimgrep.new,
    qflist_previewer = previewers.vim_buffer_qflist.new,
  },
  pickers = {
    find_files = {
      find_command = { "fd", "--type", "f", "-E", ".git/", "-E", "node_modules/", "--hidden" },
    },
    git_branches = {
      theme = "dropdown",
    },
    lsp_code_actions = {
      theme = "cursor",
    },
    live_grep = {
      theme = "ivy",
    },
  },
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown(),
    },
  },
})

wk.register({
  ["<C-p>"] = { builtin.find_files, "Find files" },
  g = {
    name = "navigation",
    r = { builtin.lsp_references, "Explore references" },
  },
})

wk.register({
  [";"] = { builtin.command_history, "Explore command history" },
  b = {
    name = "Buffers",
    e = { builtin.buffers, "Explore buffers" },
  },
  l = { builtin.resume, "Resume previous list" },
  s = {
    name = "Symbols",
    w = { builtin.lsp_workspace_symbols, "Explore workspace symbols" },
    d = { builtin.lsp_document_symbols, "Explore document symbols" },
  },
  p = {
    name = "Project",
    ["/"] = { builtin.live_grep, "Search in project" },
  },
  S = {
    name = "Source Control",
    b = { builtin.git_branches, "Branches" },
  },
}, {
  prefix = "<localleader>",
})
