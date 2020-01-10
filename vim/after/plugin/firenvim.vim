if !exists('g:started_by_firenvim')
	finish
end

set laststatus=0
set nonumber
set norelativenumber
set noruler
set noshowcmd

augroup firenvim
	au!
	au BufEnter www.github.com_*.txt set filetype=markdown.pandoc
	au BufEnter www.reddit.com_*.txt set filetype=markdown.pandoc
augroup END
