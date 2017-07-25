let g:racer_cmd = expand("$HOME/.cargo/bin/racer")

augroup racer
	au FileType rust nmap <buffer> gd <Plug>(rust-def)
	au FileType rust nmap <buffer> gs <Plug>(rust-def-split)
	au FileType rust nmap <buffer> gx <Plug>(rust-def-vertical)
	au FileType rust nmap <buffer> <leader>gd <Plug>(rust-doc)
augroup END
