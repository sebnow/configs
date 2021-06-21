if !exists('g:loaded_completion')
    finish
endif


let g:completion_enable_snippet = 'UltiSnips'
"let g:completion_matching_ignore_case = 1
"let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']
"let g:completion_sorting = "none"
"let g:completion_chain_complete_list = [
"    \{'complete_items': ['lsp', 'snippet'], 'mode': '<c-n>'},
"\]

set completeopt=menuone,noinsert,noselect
set shortmess+=c

augroup completion
	au!
	au BufEnter * lua require'completion'.on_attach()
augroup END
