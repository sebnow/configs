local completion = require('completion')
local diagnostic = require('diagnostic')
local nvim_lsp = require('nvim_lsp')

local opts = {
	on_attach = function()
		completion.on_attach()
		diagnostic.on_attach()
	end
}

nvim_lsp.rls.setup(opts)
nvim_lsp.gopls.setup(opts)
nvim_lsp.flow.setup(opts)
nvim_lsp.tsserver.setup(opts)
nvim_lsp.yamlls.setup(vim.tbl_extend("force", opts, {
	settings = {
		yaml = {
			schemas = {
				["https://raw.githubusercontent.com/awslabs/goformation/master/schema/sam.schema.json"] = "/template.yaml",
			},
		},
	},
}))
nvim_lsp.jsonls.setup(opts)
nvim_lsp.terraformls.setup(vim.tbl_extend("force", opts, {
	filetypes = {"terraform", "tf"},
	cmd = {"terraform-ls", "serve"},
}))
nvim_lsp.sumneko_lua.setup(opts)
