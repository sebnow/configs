local lspconfig = require("lspconfig")
local wk = require("which-key")

vim.filetype.add({ extension = { templ = "templ" } })

require("nvim-treesitter.configs").setup({
  ensure_installed = {}, -- Nix manages parsers
  auto_install = false,
  sync_install = false,
  ignore_install = { "all" },
  modules = {},

  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
    disable = { "yaml" },
  },
  matchup = {
    enable = true,
  },
})

require("Comment").setup()

require("conform").setup({
  formatters_by_ft = {
    lua = { "stylua" },
    javascript = { { "prettierd", "prettier" } },
    nix = { { "nixfmt", "alejandra" } },
  },
  formatters = {
    nixfmt = {
      command = "nix",
      args = { "fmt", "$FILENAME" },
      stdin = false,
      condition = function(ctx)
        return vim.fs.find({ "flake.nix" }, { upward = true, path = ctx.dirname })[1]
      end,
    },
  },
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
})

vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"

require("go").setup({
  lsp_inlay_hints = { enable = true },
})

local floating_preview_opts = {
  focusable = false,
  border = "rounded",
}

local function merge(a, b)
  return vim.tbl_extend("force", a, b)
end

local opts = {}

require("rust-tools").setup({
  tools = {
    inlay_hints = {
      auto = true,
    },
  },
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

require("markview").setup({
  preview = {
    icon_provider = "devicons",
  },
})

lspconfig.bashls.setup(opts)
lspconfig.gopls.setup(opts)
lspconfig.jsonnet_ls.setup(opts)
lspconfig.marksman.setup(opts)
lspconfig.nixd.setup(opts)
lspconfig.pylsp.setup(opts)
lspconfig.templ.setup(opts)

lspconfig.ts_ls.setup(merge(opts, {
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

lspconfig.lua_ls.setup(merge(opts, {
  settings = {
    -- This is very neovim specific but I don't currently use Lua for
    -- anything else.
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      hint = {
        enable = true,
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}))

vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Go to declaration" })
vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Go to definition" })
vim.keymap.set("n", "gI", vim.lsp.buf.implementation, { desc = "Go to implementation" })
vim.keymap.set("n", "K", function()
  require("noice.lsp").hover()
end, { desc = "Show info about the symbol under the cursor" })
vim.keymap.set("n", "<localleader>ca", vim.lsp.buf.code_action, { desc = "Show actions under cursor" })

vim.keymap.set("n", "<localleader>do", function()
  vim.diagnostic.open_float(floating_preview_opts)
end, { desc = "Show line diagnostics" })

vim.keymap.set("n", "<localleader>dn", function()
  vim.diagnostic.jump({ count = 1, float = floating_preview_opts })
end, { desc = "Go to next diagnostic" })

vim.keymap.set("n", "<localleader>dp", function()
  vim.diagnostic.jump({ count = -1, float = floating_preview_opts })
end, { desc = "Go to previous diagnostic" })

vim.keymap.set("n", "<localleader>dl", "<cmd>TroubleToggle<cr>", { desc = "Explore diagnostics" })

vim.keymap.set("n", "<localleader>fb", function()
  require("conform").format({ lsp_fallback = true })
end, { desc = "Format buffer" })

vim.keymap.set("v", "<localleader>fb", function()
  local start_row, _ = unpack(vim.api.nvim_buf_get_mark(0, "<"))
  local end_row, _ = unpack(vim.api.nvim_buf_get_mark(0, ">"))
  require("conform").format({
    lsp_fallback = true,
    range = {
      ["start"] = { start_row, 0 },
      ["end"] = { end_row, 0 },
    },
  })
end, { desc = "Format range" })

vim.keymap.set("n", "<localleader>ro", vim.lsp.buf.rename, { desc = "Rename object" })
vim.keymap.set("n", "<localleader>sw", function()
  require("telescope.builtin").lsp_workspace_symbols()
end, { desc = "Explore workspace symbols" })

vim.keymap.set("n", "<localleader>sd", function()
  require("telescope.builtin").lsp_document_symbols()
end, { desc = "Explore document symbols" })

vim.keymap.set("i", "<C-k>", vim.lsp.buf.signature_help, { desc = "Show signature help" })
