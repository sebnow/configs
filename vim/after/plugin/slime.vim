" Change the emacs-like maps to more vim-like ones
try
	vunmap <C-c><C-c>
	nunmap <C-c><C-c>
	nunmap <C-c>v
catch /^Vim\%((\a\+)\)\=:E31/
endtry

vmap <Leader>se "ry :call Send_to_Screen(@r)<CR>
nmap <Leader>sv :call Screen_Vars()<CR>

