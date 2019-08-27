let g:go_doc_keywordprg_enabled = 0
let g:go_def_mapping_enabled = 0
let g:go_metalinter_autosave = 0
let g:go_template_use_pkg = 1

let g:go_highlight_format_strings = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

let g:go_fmt_autosave = 0

augroup gomap
	" Rename Object
	au FileType go nmap <buffer> <localleader>ro :GoRename<CR>
	" Test Project
	au FileType go nmap <buffer> <localleader>tp :GoTest<CR>
	" Lint Project
	au FileType go nmap <buffer> <localleader>lp :GoMetaLinter<CR>
augroup END
