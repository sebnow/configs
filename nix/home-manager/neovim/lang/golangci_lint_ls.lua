return {
  cmd = { "golangci-lint-langserver" },
  init_options = {
    command = { "golangci-lint", "run", "--out-format", "json", "--show-stats=false" },
  },
  root_markers = { ".git", "go.mod", "go.work" },
  filetypes = { "go", "gomod" },
}
