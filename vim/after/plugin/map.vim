""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}
"
""" Denite {{{ 
if exists('g:loaded_denite')
	nnoremap <silent><C-p> :Denite buffer file/rec -start-filter -split=floating<CR>
	nnoremap <silent><leader>p/ :Denite grep<CR>
	nnoremap <silent><leader>* :DeniteCursorWord grep<CR>
	nnoremap <silent><leader>y :Denite register<CR>
endif " }}}

""" Telescope {{{
nnoremap <silent><C-p> <cmd>lua require('telescope.builtin').find_files()<CR>
nnoremap <silent><leader>be <cmd>lua require('telescope.builtin').buffers()<CR>
nnoremap <silent><leader>p/ <cmd>lua require('telescope.builtin').live_grep()<CR>
" }}}

if exists('g:nvim_lsp')
	lua require('lsp')

	function s:setup()
		setl omnifunc=v:lua.vim.lsp.omnifunc

		nnoremap <buffer><silent> <leader>do <cmd>lua vim.lsp.util.show_line_diagnostics()<CR>
		nnoremap <buffer><silent> <leader>fb <cmd>lua vim.lsp.buf.formatting()<CR>
		nnoremap <buffer><silent> <leader>ro <cmd>lua vim.lsp.buf.rename()<CR>
		nnoremap <buffer><silent> <leader>ca <cmd>lua vim.lsp.buf.code_action()<CR>
		nnoremap <buffer><silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
		nnoremap <buffer><silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
		nnoremap <buffer><silent> gI <cmd>lua vim.lsp.buf.implementation()<CR>
		nnoremap <buffer><silent> gr <cmd>lua require('telescope.builtin').lsp_references({})<CR>
		nnoremap <buffer><silent> gd <cmd>lua vim.lsp.buf.definition()<CR>

		inoremap <buffer><silent> <c+k> <cmd>lua vim.lsp.buf.signature_help()<CR>
	endfunction()

	augroup lspmap!
		au!
		au Filetype rust,go,javascript,typescript,tf call s:setup()
	augroup END
endif
