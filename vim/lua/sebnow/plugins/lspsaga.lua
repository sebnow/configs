local saga = require('lspsaga')
local wk = require('which-key')

saga.init_lsp_saga()

wk.register({
    K = {'<cmd>lua require("lspsaga.hover").render_hover_doc()<CR>', 'Show info about the symbol under the cursor'},
})

wk.register({
    c = {
        name = 'Code Actions',
        a = {'<cmd>lua require("lspsaga.codeaction").code_action()<CR>', 'Show actions under cursor'},
    },
    d = {
        o = {'<cmd>lua require("lspsaga.diagnostic").show_line_diagnostics()<CR>', 'Show line diagnostics'},
        n = {'<cmd>lua require("lspsaga.diagnostic").lsp_jump_diagnostic_next()<CR>', 'Go to next diagnostic'},
        p = {'<cmd>lua require("lspsaga.diagnostic").lsp_jump_diagnostic_prev()<CR>', 'Go to previous diagnostic'},
    },
}, {prefix = '<localleader>'})

wk.register({
    c = {
        name = 'Code Actions',
        a = {':<C-U>lua require("lspsaga.codeaction").range_code_action()<CR>', 'Show actions under cursor'},
    },
}, {prefix = '<localleader>', mode = 'v'})

wk.register({
    ['<C-k>'] = {'<cmd>lua require("lspsaga.signaturehelp").signature_help()<CR>', 'Show signature help'},
}, {mode = 'i'})
