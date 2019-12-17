" Bootstrap vim-plug
let s:vimdir = fnamemodify($MYVIMRC, ':p:h')
if empty(glob(s:vimdir.'/autoload/plug.vim'))
	execute 'silent !curl -fsLo ' s:vimdir . '/autoload/plug.vim --create-dirs' 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif
let g:LanguageClient_diagnosticsDisplay = {
    \ 1: {
		\"name": "Error",
		\"texthl": "WarningMsg",
		\"signText": "E",
		\"signTexthl": "WarningMsg",
		\},
    \2: {
		\"name": "Warning",
		\"texthl": "WarningMsg",
		\"signText": "W",
		\"signTexthl": "WarningMsg",
		\},
    \3: {
		\"name": "Info",
		\"texthl": "Guide",
		\"signText": "I",
		\"signTexthl": "Guide",
		\},
    \4: {
		\"name": "Hint",
		\"texthl": "Comment",
		\"signText": "H",
		\"signTexthl": "Comment",
		\},
    \}

runtime settings.vim
runtime map.vim

" Plugins!
call plug#begin('~/.vim/plugged')
runtime plugins.vim
""" Naughty plugin config
" These plugins don't support being configured through
" runtimepath/after/plugin
runtime before.vim
call plug#end()

runtime colorscheme.vim
