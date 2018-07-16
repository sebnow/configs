" https://vi.stackexchange.com/questions/5921/easiest-way-to-switch-git-branches/5933
function! s:changebranch(branch)
	execute 'Git checkout' . a:branch
endfunction

command! -bang Gbranch call fzf#run({
	\ 'source': 'git branch -a --no-color | grep -v "^\* " ',
	\ 'sink': function('s:changebranch')
	\ })
nmap <leader>gc :Gcommit<CR>
nmap <leader>gs :Gstatus<CR>
nmap <leader>gl :Glog<CR>
nmap <leader>gb :Gbranch<CR>
