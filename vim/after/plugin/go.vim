let g:go_doc_keywordprg_enabled = 0
let g:go_def_mapping_enabled = 0
let g:go_template_use_pkg = 1

let g:go_highlight_build_constraints = 1
let g:go_highlight_fields = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_function_parameters = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_types = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_variable_declarations = 1

let g:go_fmt_autosave = 0

augroup gomap
	" Test Project
	au FileType go nmap <buffer> <localleader>tp :GoTest ./...<CR>
	" Test Module (package)
	au FileType go nmap <buffer> <localleader>tm :GoTest<CR>
	" Lint Project
	au FileType go nmap <buffer> <localleader>lp :GoMetaLinter<CR>
augroup END

augroup go
	autocmd BufWritePre *.go :GoImports
augroup END
