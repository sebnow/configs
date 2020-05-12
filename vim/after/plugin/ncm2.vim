let g:float_preview#docked = 0

set completeopt=noinsert,menuone,noselect

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

augroup ncm2
	au!
	au BufEnter * call ncm2#enable_for_buffer()
augroup END
