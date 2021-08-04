local completion = require('completion')
local lspconfig = require('lspconfig')
local lspconfigs = require('lspconfig/configs')

function merge(a, b)
    return vim.tbl_extend("force", a, b)
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = false,
    signs = true,
    update_in_insert = false,
})

local opts = {
	on_attach = function(_client, bufnr)
        vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
		completion.on_attach()
	end
}

lspconfig.rust_analyzer.setup(vim.tbl_extend("force", opts, {
    on_attach = function(...)
        opts.on_attach(...)
        require('lsp_extensions').inlay_hints({only_current_line = true})
    end
}))

lspconfig.gopls.setup(opts)
lspconfig.flow.setup(opts)

lspconfig.tsserver.setup(merge(opts, {
    on_attach = function(client)
        client.resolved_capabilities.document_formatting = false
    end
}))

lspconfig.yamlls.setup(vim.tbl_extend("force", opts, {
	settings = {
		yaml = {
			schemas = {
				["https://raw.githubusercontent.com/awslabs/goformation/master/schema/sam.schema.json"] = "/template.yaml",
			},
		},
	},
}))
lspconfig.jsonls.setup(opts)
lspconfig.terraformls.setup(vim.tbl_extend("force", opts, {
	filetypes = {"terraform", "tf"},
	cmd = {"terraform-ls", "serve"},
}))

if not lspconfig.golangcilsp then
    lspconfigs.golangcilsp = {
        default_config = {
            cmd = {'golangci-lint-langserver'},
            root_dir = lspconfig.util.root_pattern('.git', 'go.mod'),
            init_options = {
                command = { "golangci-lint", "run", "--enable-all", "--disable", "lll", "--out-format", "json" };
            }
        };
    }
end
lspconfig.golangcilsp.setup(vim.tbl_extend("force", opts, {
	filetypes = {'go'},
}))
