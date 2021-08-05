local wk = require("which-key")

wk.register({
    g = {
        name = 'navigation',
        r = {'<cmd>lua require("telescope.builtin").lsp_references()<cr>', 'Explore references'},
    },
})

wk.register({
    [';'] = {'<cmd>lua require("telescope.builtin").command_history()<cr>', 'Explore command history'},
    f = {
        name = 'Format',
        b = {'<cmd>lua vim.lsp.buf.formatting()<cr>', 'Format buffer'},
    },
    r = {
        name = 'Rename',
        o = {'<cmd>lua vim.lsp.buf.rename()<cr>', 'Rename object'},
    },
    c = {
        name = 'Code Actions',
        l = {'<cmd>lua require("telescope.builtin").lsp_code_actions()<cr>', 'Explore actions'},
    },
    s = {
        name = 'Symbols',
        w = {'<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<cr>', 'Explore workspace symbols'},
        d = {'<cmd>lua require("telescope.builtin").lsp_document_symbols()<cr>', 'Explore document symbols'},
    },
    d = {
        name = 'Diagnostics',
        l = {'<cmd>TroubleToggle<cr>', 'Explore diagnostics'},
    },
}, {prefix = '<localleader>'})

wk.register({
    f = {
        name = 'Format',
        b = {'<cmd>lua vim.lsp.buf.range_formatting()<cr>', 'Format range'},
    },
}, {prefix = '<localleader>', mode = 'v'})
