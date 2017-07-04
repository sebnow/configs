let g:go_doc_keywordprg_enabled = 1
let g:go_def_mapping_enabled = 1

let g:go_highlight_format_strings = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

let g:go_fmt_command = "goimports"

augroup gomap
	au FileType go nmap <buffer> <localleader>ir :GoRename<CR>
	au FileType go nmap <buffer> <localleader>rt :GoTest<CR>
	au FileType go nmap <buffer> <localleader>rl :GoMetaLinter<CR>
augroup END
