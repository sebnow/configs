""" Navigation {{{
" Make scrolling faster
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
" }}}

nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> gd <Plug>(coc-definition)
nnoremap <silent> <leader>or <Plug>(coc-rename)
vnoremap <silent> <leader>ca <Plug>(coc-codeaction-selected)
nnoremap <silent> <leader>ca <Plug>(coc-codeaction-selected)

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
