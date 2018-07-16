let g:lightline = {
	\   'active': {
	\     'left':  [ [ ],
	\                [ 'paste', 'readonly', 'filename', 'modified' ],
	\                [ 'vcsbranch' ] ],
	\     'right': [ [ 'lineinfo' ],
	\                [ 'linter_errors', 'linter_warnings' ],
	\                [ 'filetype' ] ]
	\   },
	\   'inactive': {
	\     'left':  [ [ ],
	\                [ 'paste', 'readonly', 'filename', 'modified' ],
	\                [ 'vcsbranch' ] ],
	\     'right': [ [ 'lineinfo' ],
	\                [ 'linter_errors', 'linter_warnings' ],
	\                [ 'filetype' ] ]
	\   },
	\   'separator': { 'left': '', 'right': '' },
	\   'subseparator': { 'left': '', 'right': '' },
	\   'component_function': {
	\     'vcsbranch': 'VCSBranch'
	\   },
	\ }

let g:lightline.component_expand = {
	\   'linter_warnings': 'lightline#ale#warnings',
	\   'linter_errors': 'lightline#ale#errors',
	\ }

let g:lightline.component_type = {
	\   'linter_warnings': 'warning',
	\   'linter_errors': 'error',
	\ }

function! VCSBranch()
	return " " . fugitive#head()
endfunction
