let g:LanguageCleint_autoStart = 1
let g:LanguageClient_serverCommands = {
	\ 'rust': ['rustup', 'run', 'nightly', 'rls'],
	\ 'javascript': ['flow-language-server', '--stdio'],
	\ }
