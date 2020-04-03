Plug 'Yggdroot/indentLine'
Plug 'ayu-theme/ayu-vim'
Plug 'bronson/vim-trailing-whitespace'
Plug 'editorconfig/editorconfig-vim'
Plug 'fatih/vim-go', {'for': 'go', 'do': ':GoInstallBinaries'}
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
Plug 'hashivim/vim-terraform', {'for': 'terraform'}
Plug 'haya14busa/is.vim'
Plug 'honza/vim-snippets'
Plug 'itchyny/lightline.vim'
Plug 'jparise/vim-graphql'
Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }
Plug 'machakann/vim-sandwich'
Plug 'meain/vim-package-info', { 'do': 'npm install' }
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

if has('nvim')
	Plug 'neovim/nvim-lsp'
	Plug 'haorenW1025/completion-nvim'
endif
