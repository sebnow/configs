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
