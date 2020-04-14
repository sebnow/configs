if exists('g:ncm2_loaded')
	finish
endif

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
set completeopt=noinsert,menuone,noselect

augroup ncm2
	autocmd BufEnter * call ncm2#enable_for_buffer()
augroup END
