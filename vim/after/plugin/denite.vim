nnoremap <silent><c-p> :Denite file/rec<cr>
nnoremap <silent><leader>be :Denite buffer -mode=normal<cr>
nnoremap <silent><leader>p/ :Denite grep:. -mode=normal<cr>

call denite#custom#map('insert', '<C-j>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-k>', '<denite:move_to_previous_line>', 'noremap')

if exists('rg')
	call denite#custom#var('file/rec', 'command', [
		\ 'rg', '-S', '--files', '--glob', '!.git'
		\ ])

	call denite#custom#var('grep', 'command', ['rg'])
	call denite#custom#var('grep', 'default_opts', ['--hidden', '--vimgrep', '--heading', '-S'])

	" Recommended defaults for ripgrep via Denite docs
	call denite#custom#var('grep', 'recursive_opts', [])
	call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
	call denite#custom#var('grep', 'separator', ['--'])
	call denite#custom#var('grep', 'final_opts', [])
endif

if exists('fd')
	call denite#custom#var('file/rec', 'command', [
		\ 'fd', ':directory'
		\ ])
endif
