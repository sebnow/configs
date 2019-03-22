let g:ale_sign_error = '✕'
let g:ale_sign_warning = '⚑'

let g:ale_virtualtext_cursor = 1

let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')

let g:ale_linters = {
	\ 'javascript' : ['prettier-eslint', 'prettier', 'eslint'],
	\ 'go': ['golangci-lint', 'revive', 'golint'],
	\ }
let g:ale_fixers = {
	\ 'javascript' : ['prettier-eslint', 'prettier', 'eslint'],
	\ 'go' : ['goimports', 'gofmt'],
	\ 'rust': ['rustfmt'],
	\ }

call ale#linter#Define('go', {
\   'name': 'revive',
\   'output_stream': 'both',
\   'executable': 'revive',
\   'read_buffer': 0,
\   'command': 'revive %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})

call ale#linter#Define('go', {
\   'name': 'golangci-iint',
\   'output_stream': 'both',
\   'executable': 'golangci-lint',
\   'read_buffer': 0,
\   'command': 'golangci-lint run --fast %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
