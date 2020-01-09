if !has('nvim')
	finish
endif


lua <<EOF
local nvim_lsp = require('nvim_lsp');
nvim_lsp.rls.setup({})
nvim_lsp.gopls.setup({})
EOF

function s:setup()
	echo "Setting up LSP"
	setl omnifunc=v:lua.vim.lsp.omnifunc

	nnoremap <silent> <leader>fb <cmd>lua vim.lsp.buf.formatting()<CR>
	nnoremap <silent> <leader>or <cmd>lua vim.lsp.buf.rename()<CR>
	nnoremap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
	nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
	nnoremap <silent> gI <cmd>lua vim.lsp.buf.implementation()<CR>
	nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
endfunction()

augroup lspomnifunc!
	autocmd!
	autocmd Filetype rust,go call s:setup()
	autocmd BufWritePre *.rs :lua vim.lsp.buf.formatting()
augroup END

