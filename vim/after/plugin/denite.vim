nnoremap <silent><c-p> :Denite file/rec<cr>
nnoremap <silent><leader>be :Denite buffer<cr>

call denite#custom#map('insert', '<C-j>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-k>', '<denite:move_to_previous_line>', 'noremap')

if exists('fd')
	call denite#custom#var('file/rec', 'command', [
		\ 'fd', ':directory'
		\ ])
endif
