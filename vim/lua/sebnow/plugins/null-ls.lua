local nullls = require("null-ls")

nullls.setup({
  sources = {
    nullls.builtins.diagnostics.shellcheck,
    nullls.builtins.formatting.prettier,
    nullls.builtins.formatting.stylua,
    nullls.builtins.formatting.terraform_fmt,
  },
})
