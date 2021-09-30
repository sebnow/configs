local wk = require("which-key")

wk.register({
    f = {
        name = 'Format',
        b = {'<cmd>lua vim.lsp.buf.formatting()<cr>', 'Format buffer'},
    },
    r = {
        name = 'Rename',
        o = {'<cmd>lua vim.lsp.buf.rename()<cr>', 'Rename object'},
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
