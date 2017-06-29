let g:flow#enable = 0 " Disable type check on write, we have Ale for that
let g:flow#autoclose = 1

augroup flow
	au FileType javascript nmap <buffer> gD :FlowJumpToDef<CR>
augroup END
