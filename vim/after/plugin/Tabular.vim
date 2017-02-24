"
" Tabular customisation
"
if !exists('g:tabular_loaded')
	finish
endif

" Custom patterns
AddTabularPattern colon /:\zs/l1r0

" Key maps
nmap <Leader>a: :Tabularize colon<CR>

