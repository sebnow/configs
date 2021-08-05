local completion = require('completion')

vim.api.nvim_command('autocmd BufEnter * lua require("completion").on_attach()')
