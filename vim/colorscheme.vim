if has("syntax") && (&t_Co > 2 || has("gui_running"))
	syntax off
	syntax on
	set bg=dark

	set termguicolors
	let ayucolor="mirage"
	colorscheme ayu
endif
