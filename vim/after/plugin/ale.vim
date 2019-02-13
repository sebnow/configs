let g:ale_sign_error = '✕'
let g:ale_sign_warning = '⚑'

let g:ale_linters = {
	\ 'javascript' : ['prettier-eslint', 'prettier', 'eslint'],
	\ 'go': ['revive', 'golint'],
	\ }
let g:ale_fixers = {
	\ 'javascript' : ['prettier-eslint', 'prettier', 'eslint'],
	\ 'go' : ['goimports', 'gofmt'],
	\ }

call ale#linter#Define('go', {
\   'name': 'revive',
\   'output_stream': 'both',
\   'executable': 'revive',
\   'read_buffer': 0,
\   'command': 'revive %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
