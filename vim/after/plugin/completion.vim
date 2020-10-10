if !exists('g:loaded_completion')
	 finish
 endif

let g:completion_confirm_key = "\<C-y>"

set completeopt=menuone,noinsert,noselect
set shortmess+=c

augroup completion
	au!
	au BufEnter * lua require'completion'.on_attach()
augroup END
