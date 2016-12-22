command! -buffer -range=% -nargs=* Fmt call s:PerlTidy()

function! s:PerlTidy()
	let view = winsaveview()
	silent execute "%!perltidy"
	call winrestview(view)
endfunction

