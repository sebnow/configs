if !exists('g:nvim_lsp')
  finish
endif

augroup lspomnifunc!
	au!
	au BufWritePre *.rs,*.tf :lua vim.lsp.buf.formatting()
augroup END
