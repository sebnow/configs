augroup go
	autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')
augroup END
