if !exists('g:loaded_diagnostic')
	finish
endif

nnoremap <silent> <leader>dl :OpenDiagnostic<CR>
nnoremap <silent> <leader>dn :NextDiagnosticCycle<CR>
nnoremap <silent> <leader>dp :PrevDiagnosticCycle<CR>
