local treesitter = require('nvim-treesitter.configs')

treesitter.setup({
	ensure_installed = "go,rust,bash,css,graphql,html,json,javascript,nix,typescript,lua,yaml,toml",
	highlight = {
		enable = true,
	},
})
