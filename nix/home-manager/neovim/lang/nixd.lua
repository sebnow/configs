return {
  cmd = { "nixd" },
  filetypes = { "nix" },
  single_file_support = true,
  root_markers = { "flake.nix", "shell.nix" },
  settings = {
    nixd = {
      formatting = {
        command = { "nixfmt" },
      },
    },
  },
}
