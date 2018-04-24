Plug 'bronson/vim-trailing-whitespace'
Plug 'danilo-augusto/vim-afterglow'
Plug 'dhruvasagar/vim-table-mode'
Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
Plug 'editorconfig/editorconfig-vim'
Plug 'elixir-lang/vim-elixir'
Plug 'ElmCast/elm-vim'
Plug 'fatih/vim-go', {'for': 'go', 'do': ':GoInstallBinaries'}
Plug 'flowtype/vim-flow', {'for': 'javascript'}
Plug 'haya14busa/is.vim'
Plug 'IN3D/vim-raml', {'for': 'raml'}
Plug 'jlanzarotta/bufexplorer'
Plug 'jparise/vim-graphql'
Plug 'machakann/vim-sandwich'
Plug 'mileszs/ack.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'othree/html5.vim', {'for': 'html'}
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
Plug 'racer-rust/vim-racer', {'for': 'rust'}
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'slashmili/alchemist.vim'
Plug 'tpope/vim-repeat'
Plug 'unblevable/quick-scope'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'w0rp/ale'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Autocompletion
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'zchee/deoplete-go', {'for': 'go', 'do': 'make'}

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
