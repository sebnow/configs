if has("syntax") && (&t_Co > 2 || has("gui_running"))
	syntax on
	set bg=dark
	if &t_Co >= 256
		colorscheme hybrid
	elseif &t_Co >= 88
		colorscheme jellybeans
	endif
	" Reset background colour
	hi! Normal ctermbg=NONE guibg=NONE
endif
