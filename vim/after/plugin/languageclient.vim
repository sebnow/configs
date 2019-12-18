if !exists('g:LanguageClient_serverCommands')
	finish
endif


" Avoid flickering caused by column shifting. This can be removed after
set signcolumn=yes

let g:LanguageClient_settingsPath = fnamemodify($MYVIMRC, ':p:h/settings.json')
let g:LanguageClient_loadSettings = 1
let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'go': ['gopls'],
    \ }

"let g:LanguageClient_diagnosticsDisplay = {
"    \ 1: {
"		\"name": "Error",
"		\"texthl": "WarningMsg",
"		\"signText": "E",
"		\"signTexthl": "WarningMsg",
"		\},
"    \2: {
"		\"name": "Warning",
"		\"texthl": "WarningMsg",
"		\"signText": "W",
"		\"signTexthl": "WarningMsg",
"		\},
"    \3: {
"		\"name": "Info",
"		\"texthl": "Guide",
"		\"signText": "I",
"		\"signTexthl": "Guide",
"		\},
"    \4: {
"		\"name": "Hint",
"		\"texthl": "Comment",
"		\"signText": "H",
"		\"signTexthl": "Comment",
"		\},
"    \}

function s:shortcuts()
  nnoremap gd :call LanguageClient#textDocument_definition()<CR>
  nnoremap <leader>or :call LanguageClient#textDocument_rename()<CR>
  nnoremap <leader>bf :call LanguageClient#textDocument_formatting()<CR>
  nnoremap gy :call LanguageClient#textDocument_typeDefinition()<CR>
  nnoremap gr :call LanguageClient#textDocument_references()<CR>
  nnoremap <leader>la :call LanguageClient_workspace_applyEdit()<CR>
  "nnoremap <leader>lc :call LanguageClient#textDocument_completion()<CR>
  nnoremap K :call LanguageClient#textDocument_hover()<CR>
  "nnoremap <leader>ls :call LanguageClient_textDocument_documentSymbol()<CR>
  nnoremap <leader>lm :call LanguageClient_contextMenu()<CR>
endfunction()

augroup LanguageClient
  autocmd!
  autocmd FileType rust,go call s:shortcuts()
  autocmd BufWritePre *.rs,*.go call LanguageClient#textDocument_formatting()
augroup END


" Deoplete Integration {{{
if exists('g:loaded_deoplete')
	call deoplete#custom#option('ignore_sources', {
		\ 'go': ['around', 'buffer'],
		\ 'rust': ['around', 'buffer'],
		\ })
endif
" }}}
