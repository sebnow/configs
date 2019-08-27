set nocompatible
" Hide buffers instead of closing
set hidden
set nowrap
set wildmode=list:longest
set wildignore+=*.o,*.pyc,*~,*.hi,.git/**
set directory=~/.vim/swap//,.,~/tmp,/var/tmp,/tmp

" Searching {{{
set ignorecase
set smartcase
set infercase
set incsearch
set hlsearch
" }}}

" Editing {{{
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

" Completion {{{
set completeopt+=menuone
set completeopt+=noinsert
set completeopt-=preview
" }}}

" Display {{{
set title
set termencoding=utf-8
set encoding=utf-8
set showmatch
set laststatus=2
set shortmess+=acI
set ruler
set scrolloff=1
set sidescrolloff=5
set number
set relativenumber
set noshowmode

" Statusline
set statusline=%-3.3n\ %f%(\ %r%)%(\ %#WarningMsg#%m%0*%)%=(%l,\ %c)\ %P\ [%{&encoding}:%{&fileformat}]%(\ %w%)\ %y\ 

if has("folding")
	set foldenable
	" Default method. Syntax related ones should be set based on filetype
	set foldmethod=marker
	set foldlevel=1
endif
" }}}

let g:markdown_fenced_languages = ['html', 'rust', 'go', 'python', 'bash=sh', 'javascript']
