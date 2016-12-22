" Bootstrap vim-plug
let s:vimdir = fnamemodify($MYVIMRC, ':p:h')
if empty(glob(s:vimdir.'/autoload/plug.vim'))
	execute 'silent !curl -fsLo ' s:vimdir . '/autoload/plug.vim --create-dirs' 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

runtime settings.vim
runtime map.vim

" Plugins!
call plug#begin('~/.vim/plugged')
runtime plugins.vim
call plug#end()

runtime colorscheme.vim

""" Naughty plugin config
" These plugins don't support being configured through
" runtimepath/after/plugin
runtime ctrlp.vim
