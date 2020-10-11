local actions = require('telescope.actions')
local telescope = require('telescope')

telescope.setup({
    defaults = {
        mappings = {
            i = {
                ["<C-s>"] = actions.goto_file_selection_split,
                ["<C-x>"] = false,
            },
        }
    }
})
