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
