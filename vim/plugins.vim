Plug 'Yggdroot/indentLine'
Plug 'ayu-theme/ayu-vim'
Plug 'bronson/vim-trailing-whitespace'
Plug 'editorconfig/editorconfig-vim'
Plug 'fatih/vim-go', {'for': 'go', 'do': ':GoInstallBinaries'}
Plug 'hashivim/vim-terraform', {'for': 'terraform'}
Plug 'haya14busa/is.vim'
Plug 'honza/vim-snippets'
Plug 'itchyny/lightline.vim'
Plug 'jparise/vim-graphql'
Plug 'liuchengxu/vim-clap'
Plug 'machakann/vim-sandwich'
Plug 'meain/vim-package-info', { 'do': 'npm install' }
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

if has('nvim')
	Plug 'neovim/nvim-lsp'
endif
