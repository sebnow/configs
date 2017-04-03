if executable('rg')
	set grepprg=rg\ --vimgrep
elseif executable('ag')
	let g:ackprg = 'ag --vimgrep --smart-case'

	set grepprg=ag\ --nogroup\ --nocolor
endif
