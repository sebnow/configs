""" Behaviour {{{
set nocompatible
" Hide buffers instead of closing
set hidden
set nowrap
set wildmode=list,longest
set wildignore=*.o,*.pyc,*~

" Searching
set ignorecase
set smartcase
set incsearch
""" }}}

""" Editing {{{
set autoindent
set smarttab
set shiftround
set tabstop=4
set shiftwidth=4
set textwidth=72
set noexpandtab
set wrap!
set fo+=cronql1
" }}}

""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}

""" Display {{{
set title
set termencoding=utf-8
set encoding=utf-8
set lazyredraw
set showmatch
set laststatus=2
set statusline=%-3.3n\ %f%(\ %r%)%(\ %#WarningMsg#%m%0*%)%=(%l,\ %c)\ %P\ [%{&encoding}:%{&fileformat}]%(\ %w%)\ %y\ 
set shortmess+=axrI
set ruler
if has("syntax") && (&t_Co > 2 || has("gui_running"))
	syntax on
	colorscheme rdark
endif

set foldenable
" Default method. Syntax related ones should be set based on filetype
set foldmethod=marker
set foldlevel=1
" }}}

" Load pathogen. This must be done before 'filetype' is enabled
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

""" Filetype goodness {{{
filetype off " Force reload
filetype plugin indent on

if has('autocmd')
	au Syntax cpp,c,php runtime syntax/doxygen.vim
	au FileType text,latex setlocal textwidth=72 fo+=ta
	au FileType haskell,cabal setlocal expandtab
	au FileType vim setlocal keywordprg=:help
	au FileType perl setlocal iskeyword+=:
	au BufRead,BufNewFile PKGBUILD setlocal filetype=sh
	au BufRead,BufNewFile .Xdefaults* setlocal filetype=xdefaults
endif
" }}}

