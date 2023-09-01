vim.diagnostic.config({
  virtual_text = false,
  signs = true,
  update_in_insert = false,
})

vim.o.showmode = false
vim.o.title = true
vim.o.ruler = true
vim.opt.shortmess:append("acI")

vim.o.wildmode = "list:longest"
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true -- Modern languages are opinionated and tend to default to spaces
vim.o.textwidth = 72
vim.opt.formatoptions:append("cronql1jp")
vim.opt.completeopt = {"menuone", "noinsert"}

vim.o.smartcase = true
vim.o.incsearch = true

vim.o.scrolloff = 2
vim.o.sidescrolloff = 5

vim.o.statusline = "%-3.3n %f%( %r%)%( %#WarningMsg#%m%0*%)%=(%l, %c) %P [%{&encoding}:%{&fileformat}]%( %w%) %y "

vim.o.foldenable = true
vim.o.foldmethod = "marker"
vim.o.foldlevel = 99

vim.o.guifont = "Iosevka Nerd Font:h12,Iosevka:h12,monospace"
vim.o.timeoutlen = 250

vim.o.cmdheight = 0
