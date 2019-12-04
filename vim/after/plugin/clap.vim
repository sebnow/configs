if !exists('g:loaded_clap')
  finish
endif

nnoremap <silent><c-p> :<C-u>Clap files<CR>
nnoremap <silent><leader>be :<C-u>Clap buffers<CR>
nnoremap <silent><leader>p/ :<C-u>Clap grep<CR>
nnoremap <silent><leader>* :<C-u>Clap grep ++query=<cword><CR>
nnoremap <silent><leader>y :<C-u>Clap registers<CR>
