""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}


""" Completion {{{
" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" }}}

""" Telescope {{{
nnoremap <silent><C-p> <cmd>lua require('telescope.builtin').find_files()<CR>
nnoremap <silent><localleader>be <cmd>lua require('telescope.builtin').buffers()<CR>
nnoremap <silent><localleader>p/ <cmd>lua require('telescope.builtin').live_grep()<CR>
nnoremap <silent><localleader>; <cmd>lua require('telescope.builtin').command_history()<CR>
" }}}

function s:setup_lsp()
	setl omnifunc=v:lua.vim.lsp.omnifunc

	nnoremap <buffer><silent> <localleader>fb <cmd>lua vim.lsp.buf.formatting()<CR>
	nnoremap <buffer><silent> <localleader>ro <cmd>lua vim.lsp.buf.rename()<CR>
	nnoremap <buffer><silent> <localleader>ca <cmd>lua vim.lsp.buf.code_action()<CR>
	nnoremap <buffer><silent> <localleader>cl <cmd>lua require('telescope.builtin').lsp_code_actions()<CR>
	nnoremap <buffer><silent> <localleader>sw <cmd>lua require('telescope.builtin').lsp_workspace_symbols()<CR>
	nnoremap <buffer><silent> <localleader>sd <cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>
	nnoremap <buffer><silent> <localleader>cl <cmd>lua require('telescope.builtin').()<CR>
	nnoremap <buffer><silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
	nnoremap <buffer><silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
	nnoremap <buffer><silent> gI <cmd>lua vim.lsp.buf.implementation()<CR>
	nnoremap <buffer><silent> gr <cmd>lua require('telescope.builtin').lsp_references()<CR>
	nnoremap <buffer><silent> gd <cmd>lua vim.lsp.buf.definition()<CR>

	nnoremap <buffer><silent> <localleader>do <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>
	nnoremap <buffer><silent> <localleader>dl <cmd>lua vim.lsp.diagnostic.set_loclist()<CR>
	nnoremap <buffer><silent> <localleader>dn <cmd>lua vim.lsp.diagnostic.goto_next()<CR>
	nnoremap <buffer><silent> <localleader>dp <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>

	inoremap <buffer><silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
endfunction()

augroup lspmap!
	au!
	au Filetype rust,go,gomod,javascript,javascriptreact,typescript,typescriptreact,tf,yaml,lua call s:setup_lsp()
	au CursorHold,CursorHoldI *.rs :lua require('lsp_extensions').inlay_hints({only_current_line = true})
augroup END
