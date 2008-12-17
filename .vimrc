
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocp "Turn off vi compatibility mode
filetype on
filetype plugin on

set comments=sl:/*,mb:\ *,elx:\ */

set encoding=utf-8
set termencoding=utf-8

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Interface
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has('syntax') " && (&t_Co > 2)
	syntax on
endif

hi Normal ctermfg=gray
"hi Comment ctermfg=darkgray ctermbg=none
hi String ctermfg=green ctermbg=none

set ruler               " Always show current positions along the bottom
if version >= 700
    set spelllang=en_au
endif
set mouse=

"status line settings
set laststatus=2
set statusline=%-3.3n\ %f%(\ %r%)%(\ %#WarningMsg#%m%0*%)%=(%l,\ %c)\ %P\ [%{&encoding}:%{&fileformat}]%(\ %w%)\ %y\ 

set shortmess+=axr

hi StatusLine   term=inverse cterm=NONE ctermfg=white   ctermbg=black
hi StatusLineNC term=none    cterm=NONE ctermfg=darkgray ctermbg=black
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual Cues
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showmatch           " Show matching brackets
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Text Fromatting/Layout
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set textwidth=79
set wrapmargin=0
set wrap!

set fo+=c,r,o,n,q,b

au FileType mail        set fo+=a tw=72
au FileType mail,tex    set spell fo+=t
au FileType c           set omnifunc=ccomplete#Complete
" Makefiles are very strict about white space
au FileType make        set noet shiftwidth=8
au FileType php,python  set et sw=4 sts=4 ts=4
au FileType html,xhtml,phtml set fo-=t,c,a

"au FileType tex         SPCheck
"au FileType tex         let dialect='UK'

au FileType c,cpp,tex   map <F6> :make<CR>

au BufRead,BufNewFile PKGBUILD set ts=2 sts=2 et sw=2
au BufRead,BufNewFile .Xdefaults* set filetype=xdefaults

au Syntax {cpp,c,idl,php} runtime syntax/doxygen.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Folding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("folding")
    set foldenable
    set foldmethod=syntax
    set foldlevel=100
    set foldopen-=search
    set foldopen-=undo
endif

hi Folded term=standout ctermfg=3 ctermbg=0
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search & Replace
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nohlsearch
" make searches case-insensitive, unless they contain upper-case letters:
set ignorecase
set smartcase
" show the 'best match so far' as search strings are typed:
set incsearch

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Completion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set cot=menu,longest

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <F5> :!ctags -R --fields=+aiS --extra=+q .<CR>
inoremap <Nul> <C-x><C-o>
nnoremap <Left> :N<CR>
nnoremap <Right> :n<CR>

