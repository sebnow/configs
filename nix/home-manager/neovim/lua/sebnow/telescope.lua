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

vim.keymap.set("n", "<C-p>", builtin.find_files, { desc = "Find files" })
vim.keymap.set("n", "gr", builtin.lsp_references, { desc = "Explore references" })
vim.keymap.set("n", "<localleader>;", builtin.command_history, { desc = "Explore command history" })
vim.keymap.set("n", "<localleader>be", builtin.buffers, { desc = "Explore buffers" })
vim.keymap.set("n", "<localleader>l", builtin.resume, { desc = "Resume previous list" })
vim.keymap.set("n", "<localleader>sw", builtin.lsp_workspace_symbols, { desc = "Explore workspace symbols" })
vim.keymap.set("n", "<localleader>sd", builtin.lsp_document_symbols, { desc = "Explore document symbols" })
vim.keymap.set("n", "<localleader>p/", builtin.live_grep, { desc = "Search in project" })
vim.keymap.set("n", "<localleader>Sb", builtin.git_branches, { desc = "Branches" })