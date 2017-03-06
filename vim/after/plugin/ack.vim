if executable('ag')
	let g:ackprg = 'ag --vimgrep --smart-case'
	cnoreabbrev ag Ack!

	set grepprg=ag\ --nogroup\ --nocolor
endif
