" https://vi.stackexchange.com/questions/5921/easiest-way-to-switch-git-branches/5933
function! s:changebranch(branch)
	execute 'Git checkout' . a:branch
endfunction

command! -bang Gbranch call fzf#run(fzf#wrap('git_branch', {
	\ 'source': 'git branch -a --no-color | grep -v "^\* " ',
	\ 'sink': function('s:changebranch'),
	\ 'down': '20%',
	\ 'options': ['--prompt', 'Branch> '],
	\ }, <bang>0))

nmap <leader>gc :Gcommit<CR>
nmap <leader>gs :Gstatus<CR>
nmap <leader>gl :Glog<CR>
nmap <leader>gb :Gbranch<CR>
