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
    use 'hashivim/vim-terraform'
    use 'LnL7/vim-nix'
    use 'neovim/nvim-lspconfig'
    use 'nvim-lua/completion-nvim'
    use 'nvim-lua/lsp_extensions.nvim'
    use 'SirVer/ultisnips'
    use 'tpope/vim-fugitive'
    use 'wbthomason/packer.nvim'

    use {
        'camspiers/snap',
        rocks = {'fzy'},
        requires = {
            'folke/which-key.nvim',
        },
        config = function()
            require('sebnow.plugins.snap')
        end
    }

    use {
        'folke/which-key.nvim',
        config = function()
            require('sebnow.plugins.which-key')
        end
    }

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

    use {
        'jose-elias-alvarez/null-ls.nvim',
        requires = {
            'neovim/nvim-lspconfig',
            'nvim-lua/plenary.nvim',
        },
        config = function()
            require('sebnow.plugins.null-ls')
        end,
    }

    use {
        'glepnir/lspsaga.nvim',
        requires = {
            'neovim/nvim-lspconfig',
            'glepnir/lspsaga.nvim', -- For config only
        },
        config = function()
            require('sebnow.plugins.lspsaga')
        end
    }
end)

if is_fresh_install then
    packer.install()
end

return M
