augroup json
	au FileType json setlocal ts=2 sw=2 et
augroup END

command! -buffer -range=% -nargs=* Fmt call s:JsonTidy()

function! s:JsonTidy()
	let view = winsaveview()
	silent execute "%!python -m json.tool"
	call winrestview(view)
endfunction

