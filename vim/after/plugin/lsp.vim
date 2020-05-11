if !has('nvim')
	finish
endif

lua <<EOF
local nvim_lsp = require('nvim_lsp')
local diagnostic = require('diagnostic')
local ncm2 = require('ncm2')

nvim_lsp.rls.setup({on_init=ncm2.register_lsp_source, on_attach=diagnostic.on_attach})
nvim_lsp.gopls.setup({on_init=ncm2.register_lsp_source, on_attach=diagnostic.on_attach})
nvim_lsp.flow.setup({on_init=ncm2.register_lsp_source, on_attach=diagnostic.on_attach})
EOF

function s:setup()
	setl omnifunc=v:lua.vim.lsp.omnifunc

	nnoremap <silent> <leader>fb <cmd>lua vim.lsp.buf.formatting()<CR>
	nnoremap <silent> <leader>or <cmd>lua vim.lsp.buf.rename()<CR>
	nnoremap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
	nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
	nnoremap <silent> gI <cmd>lua vim.lsp.buf.implementation()<CR>
	nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
	nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
endfunction()

augroup lspomnifunc!
	autocmd!
	autocmd Filetype rust,go,javascript call s:setup()
	autocmd BufWritePre *.rs :lua vim.lsp.buf.formatting()
augroup END

