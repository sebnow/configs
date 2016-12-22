imap <C-p> <C-x><C-o>
au FileType go au BufWritePre <buffer> Fmt

" Prefer goimports over gofmt for formatting
if executable("goimports")
	let g:gofmt_command = "goimports"
endif
