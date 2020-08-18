local ncm2 = require('ncm2')
local nvim_lsp = require('nvim_lsp')

local opts = {on_init=ncm2.register_lsp_source}
nvim_lsp.rls.setup(opts)
nvim_lsp.gopls.setup(opts)
nvim_lsp.flow.setup(opts)
nvim_lsp.yamlls.setup(opts)
nvim_lsp.terraformls.setup({
	on_init = ncm2.register_lsp_source,
	filetypes = {"terraform", "tf"},
	cmd = {"terraform-ls", "serve"},
})
