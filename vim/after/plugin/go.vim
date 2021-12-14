augroup gomap
	" Test Project
	au FileType go nmap <buffer> <localleader>tp :GoTest<CR>
	" Test Module (package)
	au FileType go nmap <buffer> <localleader>tm :GoTestFile<CR>
	" Lint Project
	au FileType go nmap <buffer> <localleader>lp :GoLint<CR>
augroup END

augroup go
	autocmd BufWritePre *.go :GoImport
augroup END
