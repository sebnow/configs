function! s:colorscheme()
    try
        return g:colors_name
    catch /^Vim:E121/
        return 'default'
    endtry
endfunction

let g:lightline = {
	\   'colorscheme': s:colorscheme(),
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
	\                [ 'cocstatus' ],
	\                [ 'filetype' ] ]
	\   },
	\   'separator': { 'left': '', 'right': '' },
	\   'subseparator': { 'left': '', 'right': '' },
	\   'component_function': {
	\     'vcsbranch': 'VCSBranch',
	\     'cocstatus': 'coc#status'
	\   },
	\ }

function! VCSBranch()
	return " " . fugitive#head()
endfunction
