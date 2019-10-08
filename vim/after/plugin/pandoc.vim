if !exists("g:pandoc#syntax#codeblocks#embeds#langs")
    let g:pandoc#syntax#codeblocks#embeds#langs=[]
endif

call extend(g:pandoc#syntax#codeblocks#embeds#langs, [
	\ 'graphql',
	\ 'javascript',
	\ ])
