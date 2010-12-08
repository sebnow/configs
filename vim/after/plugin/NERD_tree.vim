"
" NERD Tree customisation
"
if !exists("loaded_nerd_tree")
	finish
endif

" NERDTree mappings
nmap <Leader>dt :NERDTreeToggle<CR>
nmap <Leader>do :NERDTree<CR>
nmap <Leader>dc :NERDTreeClose<CR>
let NERDTreeQuitOnOpen=1
let NERDTreeIgnore=['\.hi$', '\.s?o$', '\~$']
let NERDTreeWinPos="right"

