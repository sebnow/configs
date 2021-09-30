local wk = require("which-key")
local actions = require('telescope.actions')
local previewers = require('telescope.previewers')
local sorters = require('telescope.sorters')
local telescope = require('telescope')

telescope.setup({
    defaults = {
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
    }
})

wk.register({
    g = {
        name = 'navigation',
        r = {'<cmd>lua require("telescope.builtin").lsp_references()<cr>', 'Explore references'},
    },
})

wk.register({
    [';'] = {'<cmd>lua require("telescope.builtin").command_history()<cr>', 'Explore command history'},
    c = {
        name = 'Code Actions',
        l = {'<cmd>lua require("telescope.builtin").lsp_code_actions()<cr>', 'Explore actions'},
    },
    s = {
        name = 'Symbols',
        w = {'<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<cr>', 'Explore workspace symbols'},
        d = {'<cmd>lua require("telescope.builtin").lsp_document_symbols()<cr>', 'Explore document symbols'},
    },
}, {prefix = '<localleader>'})
