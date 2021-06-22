local wk = require("which-key")

wk.register({
    ['<C-p>'] = {'<cmd>lua require("telescope.builtin").find_files({find_command = {"fd", "--type", "f", "-H", "-E", ".git"}})<cr>', 'Explore files'},
})

wk.register({
    K = {'<cmd>lua vim.lsp.buf.hover()<cr>', 'Show info about the symbol under the cursor'},
    g = {
        name = 'navigation',
        D = {'<cmd>lua vim.lsp.buf.declaration()<cr>', 'Go to declaration'},
        I = {'<cmd>lua vim.lsp.buf.implementation()<cr>', 'Go to implementation'},
        d = {'<cmd>lua vim.lsp.buf.definition()<cr>', 'Go to definition'},
        r = {'<cmd>lua require("telescope.builtin").lsp_references()<cr>', 'Explore references'},
    },
}, {buffer = 0})

wk.register({
    b = {
        name = 'buffers',
        e = {'<cmd>Telescope buffers<cr>', 'Explore buffers' },
    },
    p = {
        name = 'project',
        ['/'] = {'<cmd>lua require("telescope.builtin").live_grep()<cr>', 'Search in project'},
    },
    [';'] = {'<cmd>lua require("telescope.builtin").command_history()<cr>', 'Explore command history'},
}, {prefix = '<localleader>'})

wk.register({
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
        a = {'<cmd>lua vim.lsp.buf.code_action()<cr>', 'Show actions under cursor'},
        l = {'<cmd>lua require("telescope.builtin").lsp_code_actions()<cr>', 'Explore actions'},
    },
    s = {
        name = 'Symbols',
        w = {'<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<cr>', 'Explore workspace symbols'},
        d = {'<cmd>lua require("telescope.builtin").lsp_document_symbols()<cr>', 'Explore document symbols'},
    },
    d = {
        name = 'Diagnostics',
        o = {'<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>', 'Show line diagnostics'},
        l = {'<cmd>lua vim.lsp.diagnostic.set_loclist()<cr>', 'Explore diagnostics'},
        n = {'<cmd>lua vim.lsp.diagnostic.goto_next()<cr>', 'Go to next diagnostic'},
        p = {'<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>', 'Go to previous diagnostic'},
    },
}, {prefix = '<localleader>', buffer = 0})

wk.register({
    ['<C-k>'] = {'<cmd>lua vim.lsp.buf.signature_help()<cr>', 'Show signature help'},
}, {buffer = 0, mode = 'i'})
