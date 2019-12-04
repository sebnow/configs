if !exists('g:did_coc_loaded')
  finish
endif

let g:coc_global_extensions = [
			\ 'coc-diagnostic',
			\ 'coc-eslint',
			\ 'coc-json',
			\ 'coc-lists',
			\ 'coc-prettier',
			\ 'coc-project',
			\ 'coc-rls',
			\ 'coc-snippets',
			\ 'coc-yaml',
			\ 'coc-yank',
			\ 'coc-git',
			\ 'coc-go',
			\ ]

nnoremap <silent> K :call CocAction('doHover')<CR>
inoremap <silent><expr> <TAB>
			\ pumvisible() ? "\<C-n>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Navigation maps
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gI <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Object/Symbol maps
nmap <leader>or <Plug>(coc-rename)
nnoremap <silent> <leader>ol :<C-u>CocList outline<CR>
nnoremap <silent> <leader>o/ :<C-u>CocList -I symbols<CR>
vnoremap <silent> <leader>ca <Plug>(coc-codeaction-selected)
nnoremap <silent> <leader>ca <Plug>(coc-codeaction-selected)
nnoremap <silent> K :call <SID>show_documentation()<CR>

nmap <leader>bf <Plug>(coc-format)
vmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

" Snippets
imap <C-j> <Plug>(coc-snippets-expand-jump)

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
