let g:coc_global_extensions = ['coc-rls', 'coc-json', 'coc-snippets', 'coc-prettier', 'coc-eslint']

nnoremap <silent> K :call CocAction('doHover')<CR>

" Navigation maps
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gI <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Object/Symbol maps
nmap <leader>or <Plug>(coc-rename)
nnoremap <silent> <leader>ol :<C-u>CocList outline<CR>
nnoremap <silent> <leader>o/ :<C-u>CocList -I symbols<CR>
