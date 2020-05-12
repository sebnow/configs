local diagnostic = require('diagnostic')
local ncm2 = require('ncm2')
local nvim_lsp = require('nvim_lsp')

nvim_lsp.rls.setup({on_init=ncm2.register_lsp_source, on_attach=diagnostic.on_attach})
nvim_lsp.gopls.setup({on_init=ncm2.register_lsp_source, on_attach=diagnostic.on_attach})
nvim_lsp.flow.setup({on_init=ncm2.register_lsp_source, on_attach=diagnostic.on_attach})
