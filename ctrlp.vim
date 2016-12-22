" {{{ CtrlP
let g:ctrlp_extensions = ['mixed']
" Only change directory if working within a "project"
" (there's a VCS repo)
let g:ctrlp_working_path_mode = 'ra'
" Search files, buffers and MRU by default
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard']
let g:ctrlp_custom_ignore = {
	\ 'file': '\v\.class$',
	\ }
" }}}

