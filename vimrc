""" Behaviour {{{
set nocompatible
" Hide buffers instead of closing
set hidden
set nowrap
set wildmode=list:longest
set wildignore+=*.o,*.pyc,*~,*.hi,.git/**

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
if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif
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
set showmatch
set laststatus=2
set shortmess+=axrI
set ruler

" Statusline
if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif
let g:airline_symbols.linenr = ''
" TODO: Put this in gvimrc
if has('gui_running')
	let g:airline_powerline_fonts=1
endif
set statusline=%-3.3n\ %f%(\ %r%)%(\ %#WarningMsg#%m%0*%)%=(%l,\ %c)\ %P\ [%{&encoding}:%{&fileformat}]%(\ %w%)\ %y\

if has("folding")
	set foldenable
	" Default method. Syntax related ones should be set based on filetype
	set foldmethod=marker
	set foldlevel=1
endif
" }}}

" Plugins!
call plug#begin('~/.vim/plugged')
Plug 'Blackrush/vim-gocode'
Plug 'Rip-Rip/clang_complete'
Plug 'Townk/vim-autoclose'
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'
Plug 'chriskempson/base16-vim'
Plug 'godlygeek/tabular'
Plug 'kien/ctrlp.vim'
Plug 'marijnh/tern_for_vim'
Plug 'msanders/snipmate.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'rodjek/vim-puppet'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/bufexplorer.zip'
Plug 'vim-scripts/matchit.zip'
Plug 'vim-scripts/vim-less'
Plug 'wting/rust.vim'
call plug#end()

""" Filetype goodness {{{
filetype off " Force reload
filetype plugin indent on

" Load color scheme after pathogen so the RTP is correct
if has("syntax") && (&t_Co > 2 || has("gui_running"))
	syntax on
	set bg=dark
	if &t_Co >= 88
		colorscheme jellybeans
	elseif &t_Co >= 256
		colorscheme base16-tomorrow
	endif
	" Reset background colour
	"hi! Normal ctermbg=NONE guibg=NONE
endif

if has("python")
	let g:clang_use_library = 1
endif

if has('autocmd')
	au Syntax cpp,c,php runtime syntax/doxygen.vim
	au FileType text,latex setlocal textwidth=72 fo+=ta
	au FileType haskell,cabal setlocal expandtab makeprg=cabal\ build
	au FileType vim setlocal keywordprg=:help
	au FileType perl setlocal iskeyword+=:
	au FileType markdown,gitcommit setl spell
	au BufRead,BufNewFile PKGBUILD setlocal filetype=sh
	au BufRead,BufNewFile .Xdefaults* setlocal filetype=xdefaults
endif
" }}}

""" Naughty plugin config
" These plugins don't support being configured through
" runtimepath/after/plugin

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

