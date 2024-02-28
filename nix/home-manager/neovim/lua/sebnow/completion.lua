local cmp = require("cmp")
require("copilot").setup({
  suggestion = { enabled = false },
  panel = { enabled = false },
})

vim.api.nvim_create_autocmd("InsertEnter", {
  callback = function()
    require("copilot_cmp").setup()
  end,
})

vim.g.copilot_no_tab_map = true

cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<C-y>"] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
  }),
  experimental = {
    ghost_text = true,
  },
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "copilot" },
    { name = "buffer", keyword_length = 4 },
    { name = "path" },
  }),
  snippet = {
    expand = function(args)
      vim.snippet.expand(args.body)
    end,
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
})
