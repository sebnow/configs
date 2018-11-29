let g:LanguageCleint_autoStart = 1
let g:LanguageClient_serverCommands = {
	\ 'rust': ['rls'],
	\ 'javascript': ['flow-language-server', '--stdio'],
	\ 'go': ['go-langserver', '-gocodecompletion'],
	\ }
