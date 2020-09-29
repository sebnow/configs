if !exists('g:nvim_lsp')
  finish
endif

lua require "lsp"

augroup lspomnifunc!
	au!
	au BufWritePre *.rs,*.tf :lua vim.lsp.buf.formatting()
augroup END
