if !exists('g:nvim_lsp')
  finish
endif

lua require "lsp"

function s:setup()
	setl omnifunc=v:lua.vim.lsp.omnifunc

	nnoremap <silent> <leader>fb <cmd>lua vim.lsp.buf.formatting()<CR>
	nnoremap <silent> <leader>ro <cmd>lua vim.lsp.buf.rename()<CR>
	nnoremap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
	nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
	nnoremap <silent> gI <cmd>lua vim.lsp.buf.implementation()<CR>
	nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
	nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
endfunction()

augroup lspomnifunc!
	au!
	au Filetype rust,go,javascript,tf call s:setup()
	au BufWritePre *.rs,*.tf :lua vim.lsp.buf.formatting()
augroup END
