local wk = require("which-key")
local snap = require('sebnow.plugins.snap')

wk.register({
    ['<C-p>'] = {snap.find_files, 'Explore files'},
    g = {
        name = 'navigation',
        D = {'<cmd>lua vim.lsp.buf.declaration()<cr>', 'Go to declaration'},
        I = {'<cmd>lua vim.lsp.buf.implementation()<cr>', 'Go to implementation'},
        d = {'<cmd>lua vim.lsp.buf.definition()<cr>', 'Go to definition'},
        r = {'<cmd>lua require("telescope.builtin").lsp_references()<cr>', 'Explore references'},
    },
})

wk.register({
    [';'] = {'<cmd>lua require("telescope.builtin").command_history()<cr>', 'Explore command history'},
    b = {
        name = 'buffers',
        e = {snap.buffers, 'Explore buffers' },
    },
    p = {
        name = 'project',
        ['/'] = {snap.grep, 'Search in project'},
    },
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
