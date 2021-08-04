local nullls = require('null-ls')

nullls.config({
    sources = {
        nullls.builtins.diagnostics.shellcheck,
        nullls.builtins.formatting.prettier,
        nullls.builtins.formatting.stylua,
        nullls.builtins.formatting.terraform_fmt,
    },
})

require("lspconfig")["null-ls"].setup({})
