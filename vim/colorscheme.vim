if has("syntax") && (&t_Co > 2 || has("gui_running"))
	syntax on
	set bg=dark
	colorscheme afterglow
	" Reset background colour
	hi! Normal ctermbg=NONE guibg=NONE
endif
