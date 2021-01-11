augroup neoformat
  autocmd!
  autocmd BufWritePre *.js,*.ts,*.tsx,*.jsx undojoin | Neoformat
augroup END
