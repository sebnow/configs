local diagnostic = require('diagnostic')
local nvim_lsp = require('nvim_lsp')

local opts = {
	on_init = ncm2.register_lsp_source,
	on_attach = diagnostic.on_attach,
}

nvim_lsp.rls.setup(opts)
nvim_lsp.gopls.setup(opts)
nvim_lsp.flow.setup(opts)
nvim_lsp.yamlls.setup({
	on_init = opts.on_init,
	on_attach = opts.on_attach,
	settings = {
		yaml = {
			schemas = {
				["https://raw.githubusercontent.com/awslabs/goformation/master/schema/sam.schema.json"] = "/template.yaml",
			},
		},
	},
})
nvim_lsp.jsonls.setup(opts)
nvim_lsp.terraformls.setup({
	on_attach = opts.on_attach,
	filetypes = {"terraform", "tf"},
	cmd = {"terraform-ls", "serve"},
})
