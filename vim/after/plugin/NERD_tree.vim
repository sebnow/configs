"
" NERD Tree customisation
"
if !exists("loaded_nerd_tree")
	finish
endif

" NERDTree mappings
nmap <Leader>tt :NERDTreeToggle<CR>
nmap <Leader>to :NERDTree<CR>
nmap <Leader>tc :NERDTreeClose<CR>
let NERDTreeQuitOnOpen=1
let NERDTreeIgnore=['\.hi$', '\.s?o$', '\~$']
let NERDTreeWinPos="right"

