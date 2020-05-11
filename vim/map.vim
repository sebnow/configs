""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}

""" Denite {{{
nnoremap <C-p> :Denite buffer file/rec -start-filter -split=floating<CR>
nnoremap <leader>p/ :Denite grep<CR>
nnoremap <leader>* :DeniteCursorWord grep<CR>
nnoremap <leader>y :Denite register<CR>
" }}}
