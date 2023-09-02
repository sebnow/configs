return {
  config = function()
    local nullls = require("null-ls")

    nullls.setup({
      sources = {
        nullls.builtins.formatting.prettier,
        nullls.builtins.formatting.stylua,
      },
    })
  end,
}
