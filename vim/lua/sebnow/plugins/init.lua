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
    use 'nvim-lua/lsp_extensions.nvim'
    use 'SirVer/ultisnips'
    use 'tpope/vim-fugitive'
    use 'wbthomason/packer.nvim'

    use {
        'nvim-lua/completion-nvim',
        config = function()
            require('sebnow.plugins.completion')
        end,
    }

    use {
        'neovim/nvim-lspconfig',
        requires = {
            'folke/which-key.nvim',
        },
        config = function()
            require('sebnow.plugins.lsp')
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
        branch = '0.5-compat',
        run = ':TSUpdate',
    }

    use {
        'plasticboy/vim-markdown',
        ft = {'markdown'},
    }

    use {
        'nvim-telescope/telescope.nvim',
        commit = '5d37c3ea08f40d8c9d3a9ebcc72bd641d366c110',
        branch = 'master',
        requires = {
            'kyazdani42/nvim-web-devicons',
            'nvim-lua/plenary.nvim',
            'nvim-lua/popup.nvim',
            'folke/which-key.nvim',
        },
        config = function()
            require('sebnow.plugins.telescope')
        end,
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
end)

if is_fresh_install then
    packer.install()
end

return M
