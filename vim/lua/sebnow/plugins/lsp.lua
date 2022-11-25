local lspconfig = require("lspconfig")
local lspconfigs = require("lspconfig/configs")
local wk = require("which-key")

function merge(a, b)
  return vim.tbl_extend("force", a, b)
end

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "rounded",
})

local opts = {
  -- TODO: Refactor this so that the completion plugin is decoupled
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
  on_attach = function(_client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  end,
}

require("rust-tools").setup({
  server = merge(opts, {
    root_dir = require("lspconfig.util").root_pattern("Cargo.toml"),
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          allFeatures = true,
          overrideCommand = {
            "cargo",
            "clippy",
            "--workspace",
            "--message-format=json",
            "--all-targets",
            "--all-features",
          },
        },
      },
    },
  }),
})

lspconfig.gopls.setup(opts)
lspconfig.flow.setup(opts)
lspconfig.svelte.setup(opts)

lspconfig.tsserver.setup(merge(opts, {
  on_attach = function(client)
    client.resolved_capabilities.document_formatting = false
  end,
  filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
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
  filetypes = { "terraform", "tf" },
  cmd = { "terraform-ls", "serve" },
}))

lspconfig.golangci_lint_ls.setup(opts)

wk.register({
  g = {
    name = "Navigation",
    D = { vim.lsp.buf.declaration, "Go to declaration" },
    I = { vim.lsp.buf.implementation, "Go to implementation" },
    d = { vim.lsp.buf.definition, "Go to definition" },
  },
  K = { "<cmd>lua vim.lsp.buf.hover()<cr>", "Show info about the symbol under the cursor" },
})

wk.register({
  c = {
    name = "Code Actions",
    a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Show actions under cursor" },
  },
  d = {
    o = {
      '<cmd>lua vim.diagnostic.open_float({scope="line",focusable=false,border="rounded"})<cr>',
      "Show line diagnostics",
    },
    n = {
      "<cmd>lua vim.diagnostic.goto_next()<cr>",
      "Go to next diagnostic",
    },
    p = {
      "<cmd>lua vim.diagnostic.goto_prev()<cr>",
      "Go to previous diagnostic",
    },
  },
  f = {
    name = "Format",
    b = { "<cmd>lua vim.lsp.buf.format({async=true})<cr>", "Format buffer" },
  },
  r = {
    name = "Rename",
    o = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename object" },
  },
  s = {
    name = "Symbols",
    w = { '<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<cr>', "Explore workspace symbols" },
    d = { '<cmd>lua require("telescope.builtin").lsp_document_symbols()<cr>', "Explore document symbols" },
  },
}, {
  prefix = "<localleader>",
})

wk.register({
  c = {
    name = "Code Actions",
    a = { ":<C-U>lua vim.lsp.buf.range_code_action()<cr>", "Show actions for a given range" },
  },
  f = {
    name = "Format",
    b = { "<cmd>lua vim.lsp.buf.range_formatting()<cr>", "Format range" },
  },
}, {
  prefix = "<localleader>",
  mode = "v",
})

wk.register({
  ["<C-k>"] = { "<cmd>lua vim.lsp.buf.signature_help()<Cr>", "Show signature help" },
}, { mode = "i" })
