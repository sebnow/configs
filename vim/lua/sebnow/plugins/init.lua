local is_fresh_install = false
local install_path = vim.call('stdpath', 'data') .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  is_fresh_install = true
  vim.cmd('packadd packer.nvim')
end

vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])

local packer = require('packer')

local M = packer.startup(function()
    use 'aklt/plantuml-syntax'
    use 'fatih/vim-go'
    use 'folke/which-key.nvim'
    use 'hashivim/vim-terraform'
    use 'LnL7/vim-nix'
    use 'neovim/nvim-lspconfig'
    use 'nvim-lua/completion-nvim'
    use 'nvim-lua/lsp_extensions.nvim'
    use 'sbdchd/neoformat'
    use 'SirVer/ultisnips'
    use 'tpope/vim-fugitive'
    use 'wbthomason/packer.nvim'
    use { 'camspiers/snap', rocks = {'fzy'}}

    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
    }

    use {
        'plasticboy/vim-markdown',
        ft = {'markdown'},
    }

    use {
        'nvim-telescope/telescope.nvim',
        commit = 'feaed4b6e23bd56906089154f293f2b1ecb68c7e',
        requires = {
            'nvim-lua/popup.nvim',
            'nvim-lua/plenary.nvim',
        },
    }

    use {
        'Shatur/neovim-ayu',
        config = function()
            vim.g.ayu_mirage = true
            vim.cmd('colorscheme ayu')
        end
    }

    use {
        'folke/trouble.nvim',
        requires = {
            'kyazdani42/nvim-web-devicons',
        },
    }
end)

if is_fresh_install then
    packer.install()
end

require('sebnow.plugins.which-key')
require('sebnow.plugins.snap')

return M
