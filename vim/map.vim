""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}

""" Denite {{{
nnoremap <silent><C-p> :Denite buffer file/rec -start-filter -split=floating<CR>
nnoremap <silent><leader>p/ :Denite grep<CR>
nnoremap <silent><leader>* :DeniteCursorWord grep<CR>
nnoremap <silent><leader>y :Denite register<CR>
" }}}
