""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}


""" Completion {{{
" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" }}}
