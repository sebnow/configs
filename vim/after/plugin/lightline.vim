function! s:colorscheme()
    try
        return g:colors_name
    catch
        return 'default'
    endtry
endfunction

let g:lightline = {
	\   'colorscheme': s:colorscheme(),
	\   'active': {
	\     'left':  [ [ ],
	\                [ 'paste', 'readonly', 'filename', 'modified' ],
	\                [ 'vcsbranch' ] ],
	\     'right': [ [ 'currentfunction', 'lineinfo' ],
	\                [ 'cocstatus', 'linter_errors', 'linter_warnings' ],
	\                [ 'filetype' ] ]
	\   },
	\   'inactive': {
	\     'left':  [ [ ],
	\                [ 'paste', 'readonly', 'filename', 'modified' ],
	\                [ 'vcsbranch' ] ],
	\     'right': [ [ 'currentfunction', 'lineinfo' ],
	\                [ 'cocstatus' ],
	\                [ 'filetype' ] ]
	\   },
	\   'separator': { 'left': '', 'right': '' },
	\   'subseparator': { 'left': '', 'right': '' },
	\   'component_function': {
	\     'vcsbranch': 'VCSBranch',
	\     'cocstatus': 'coc#status',
	\     'currentfunction': 'CocCurrentFunction'
	\   },
	\ }

function! VCSBranch()
	return " " . fugitive#head()
endfunction

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction
